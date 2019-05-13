
module I = struct
  (*************)
  (* influxdb line protocol reporter *)
  (* from https://docs.influxdata.com/influxdb/v1.5/write_protocols/line_protocol_reference/ *)
  (* example line: weather,location=us-midwest temperature=82 1465839830100400200 *)
  (*************)

  open Astring

  let avoid_keyword =
    let keywords = String.Set.of_list [
        "ALL" ; "ALTER" ; "ANY" ; "AS" ; "ASC" ; "BEGIN" ;
        "BY" ; "CREATE" ; "CONTINUOUS" ; "DATABASE" ; "DATABASES" ; "DEFAULT" ;
        "DELETE" ; "DESC" ; "DESTINATIONS" ; "DIAGNOSTICS" ; "DISTINCT" ; "DROP" ;
        "DURATION" ; "END" ; "EVERY" ; "EXPLAIN" ; "FIELD" ; "FOR" ;
        "FROM" ; "GRANT" ; "GRANTS" ; "GROUP" ; "GROUPS" ; "IN" ;
        "INF" ; "INSERT" ; "INTO" ; "KEY" ; "KEYS" ; "KILL" ;
        "LIMIT" ; "SHOW" ; "MEASUREMENT" ; "MEASUREMENTS" ; "NAME" ; "OFFSET" ;
        "ON" ; "ORDER" ; "PASSWORD" ; "POLICY" ; "POLICIES" ; "PRIVILEGES" ;
        "QUERIES" ; "QUERY" ; "READ" ; "REPLICATION" ; "RESAMPLE" ; "RETENTION" ;
        "REVOKE" ; "SELECT" ; "SERIES" ; "SET" ; "SHARD" ; "SHARDS" ;
        "SLIMIT" ; "SOFFSET" ; "STATS" ; "SUBSCRIPTION" ; "SUBSCRIPTIONS" ; "TAG" ;
        "TO" ; "USER" ; "USERS" ; "VALUES" ; "WHERE" ; "WITH" ; "WRITE"
      ] in
    (fun m -> if String.(Set.mem (Ascii.uppercase m) keywords) then "o" ^ m else m)

  let escape =
    List.fold_right (fun e m' -> String.(concat ~sep:("\\" ^ e) (cuts ~sep:e m')))

  let escape_measurement m = escape  [ "," ; " " ] (avoid_keyword m)

  let escape_name m = escape [ "," ; " " ; "=" ] (avoid_keyword m)

  let pp_value (str : string Fmt.t) ppf f =
    let open Metrics in
    match value f with
    | V (String, s) -> str ppf s
    | V (Int, i) -> Fmt.pf ppf "%di" i
    | V (Int32, i32) -> Fmt.pf ppf "%ldi" i32
    | V (Int64, i64) -> Fmt.pf ppf "%Ldi" i64
    | V (Uint, u) -> Fmt.pf ppf "%ui" u
    | V (Uint32, u32) -> Fmt.pf ppf "%lui" u32
    | V (Uint64, u64) -> Fmt.pf ppf "%Lui" u64
    | _ -> pp_value ppf f

  (* we need to:
     - avoid keywords
     - escape comma and space in measurement name
     - escape comma, space and equal in tag key, tag value, field key of type string
     - double-quote field value of type string
     - data type number is a float, suffix i for integers *)
  let encode_line_protocol tags data name =
    let data_fields = Metrics.Data.fields data in
    let pp_field_str ppf s = Fmt.pf ppf "%S" s in
    let pp_field ppf f =
      Fmt.(pair ~sep:(unit "=") string (pp_value pp_field_str)) ppf
        (escape_name (Metrics.key f), f)
    in
    let pp_fields = Fmt.(list ~sep:(unit ",") pp_field) in
    let pp_tag_str ppf s = Fmt.string ppf (escape_name s) in
    let pp_tag ppf f =
      Fmt.(pair ~sep:(unit "=") string (pp_value pp_tag_str)) ppf
        (escape_name (Metrics.key f), f)
    in
    let pp_tags = Fmt.(list ~sep:(unit ",") pp_tag) in
    Fmt.strf "%s,%a %a\n" (escape_measurement name) pp_tags tags pp_fields data_fields
end

module R = struct
  module SM = Map.Make(Metrics.Src)

  let store_reporter now () =
    let m = ref SM.empty in
    let report ~tags ~data ~over src k =
      m := SM.add src (tags, data) !m;
      over (); k ()
    in
    (fun () -> !m), { Metrics.report ; now ; at_exit = (fun () -> ()) }
end

module L = struct
  open Lwt.Infix

  let gc_stat = Metrics.gc_stat ~tags:Metrics.Tags.[]

  let log_stats ~tags =
    let open Metrics in
    let doc = "Statistics of the Logs library" in
    let data () =
      let warnings, errors = Logs.warn_count (), Logs.err_count () in
      Data.v
        [ int "warnings" warnings ; int "errors" errors ]
    in
    Src.v ~doc ~tags ~data "logs"

  let log_stat = log_stats ~tags:Metrics.Tags.[]

  let malloc_stat = OS.MM.malloc_metrics ~tags:Metrics.Tags.[]

  let collect s () =
    let f () = Metrics.add gc_stat (fun x -> x) (fun d -> d ())
    and g () = Metrics.add log_stat (fun x -> x) (fun d -> d ())
    and h () = Metrics.add malloc_stat (fun x -> x) (fun d -> d ())
    in
    let one () = f (); g (); h (); Lwt.return_unit in
    let rec loop () = Lwt.join [ one (); s () ] >>= loop in
    loop ()
end

module M = struct
  open Lwt.Infix

  let src = Logs.Src.create "influx" ~doc:"influx metrics reporter"
  module Log = (val Logs.src_log src : Logs.LOG)

  let vmname =
    Metrics.field
      ~doc:"name of the virtual machine"
      "vm" Metrics.String

  module Pull (T : Mirage_time_lwt.S) (C : Mirage_clock.MCLOCK) (F : Mirage_flow_lwt.S) = struct

    type t = { mutable flows : F.flow list }

    let add_flow t flow =
      Log.debug (fun m -> m "registered new flow");
      t.flows <- flow :: t.flows

    let timer get host t =
      let datas =
        R.SM.fold (fun src (tags, data) acc ->
            let name = Metrics.Src.name src in
            I.encode_line_protocol (host@tags) data name :: acc)
          (get ()) []
      in
      let datas = String.concat "" datas in
      Log.debug (fun m -> m "sending measurements to %d flows %s" (List.length t.flows) datas);
      let cs = Cstruct.of_string datas in
      Lwt_list.fold_left_s (fun acc flow ->
          F.write flow cs >|= function
          | Ok () -> flow :: acc
          | Error we ->
            Log.warn (fun m -> m "writing to flow failed %a, dropping" F.pp_write_error we);
            acc)
        [] t.flows >|= fun flows ->
      t.flows <- flows

    let timer_loop get host interval t () =
      let rec one () =
        Lwt.join [
          timer get host t;
          T.sleep_ns (Duration.of_sec interval)
        ] >>= fun () ->
        (one[@tailcall]) ()
      in
      one ()

    let create ?(interval = 10) ?hostname () =
      let get, reporter = R.store_reporter C.elapsed_ns () in
      Metrics.set_reporter reporter;
      let host = match hostname with None -> [] | Some host -> [vmname host] in
      let t = { flows = [] } in
      Lwt.async (timer_loop get host interval t);
      t
  end

  module S (T : Mirage_time_lwt.S) (P : Mirage_clock.PCLOCK) (C : Mirage_clock.MCLOCK) (S : Mirage_stack_lwt.V4) = struct
    module TLS = Tls_mirage.Make(S.TCPV4)
    module I = Pull(T)(C)(TLS)

    let ca =
      let data = Cstruct.of_string {|
-----BEGIN CERTIFICATE-----
MIIFFzCCAv+gAwIBAgIJAMWQyAG/b/OyMA0GCSqGSIb3DQEBCwUAMBgxFjAUBgNV
BAMMDW1vbml0b3JpbmctY2EwHhcNMTkwNTEyMjA1OTQ5WhcNMjkwNTA5MjA1OTQ5
WjAYMRYwFAYDVQQDDA1tb25pdG9yaW5nLWNhMIICIjANBgkqhkiG9w0BAQEFAAOC
Ag8AMIICCgKCAgEAwczrhNy2F4sVdvo+YMWKtrvPUyRphxLvkFPmO9ewhQIXEdW/
cq+DRKCennOH8F5/mb848MtJDV7plqap+QD3Pzn8t4TjlVd7l1sPjnwLcpeFvtWn
k9fxcsFQkl+D2nkCvQJIYlS9UWEq6d8BeVFt07aqN8k/Q3CqdBrJNVjX4g6Fo11h
4lG8BZLRBU+Sesq7+5TKw3nZPenMfcsLjmHGA0Xkxk687tb2ONkvH9OsT0pHnf2W
M9WczHxtBJKI1QtxMb4gOeSMFV3FnfPbswt9H+B5tzu2YjV2H/fXVtejJhgBMmlg
pIbccyVoKcjJXaQylLsCjPOxIiqtElTHl2rgOZxwacrewBvE/PX2ulDVCfLZGM+h
90tpLgO5JQ+nlfQUgtb8W4JP8gO2BFPBIOuf394J/jrGOfFL6gOAUmNvYoYm66I9
OW+wE8FvLAR2AItkxTvwUQHspt7PrlFMqMAaWYpG7d4tpOAZTkCKiBMQovZzC0Qt
IFvw5Obo5BbSM7ZCgWseIy6nCeM1F4HDxWTWfYETAKN2p6iTjbQ9KHQfOB7v18iI
zUifJAgeRD09MW0uJa1/8MJDGniPV+dLTy3unnYyuq22tQzv4lr1e6sQsNcnVI8n
+RVkYKBrIQeC8Y+VAsKr/pjzEFiglbZ26mvyKXHdssVWePzHA2Wqc7SAfIkCAwEA
AaNkMGIwHwYDVR0jBBgwFoAUm1qoqdDxv45dJj8zxOX5aQSVSngwHQYDVR0OBBYE
FJtaqKnQ8b+OXSY/M8Tl+WkElUp4MA8GA1UdEwEB/wQFMAMBAf8wDwYDVR0PAQH/
BAUDAwfGADANBgkqhkiG9w0BAQsFAAOCAgEAwZpZq7MAjO7f/aPxycEYzPrAMiO/
dIak48w9fTVlrVT8AgKfIKtsygr6lpKLe2aJJwEVuq8TaVhPVVi8cfS1DVMB6ifx
SWA+yv//C1tdt0LiswD4nE55418+c2RvRdlH6lVjzeYEOis+qY/MhAj33iSQ071E
fPCPeiNa9P5vIu6Ds1FAZZ+3W0h7nUyhZGjUBngeT5FdBzjRZBPqloeBCkftYpz7
HaNUZbRt+N/1m4bUPaUzm+jI3u3sxrglJnLgr+vXW4seCv1tzi9GTTl/DkTVXoiU
DEQOHwGC/NzqezguBey5M7M9fiqgIbEFaXBPMUvmDkYKrDX/L1z4TxOiNZupxEky
RavawY8Do6OISqkhmPTvDa1N82asnLmKHEnP8JoSZRiTORqpaJgdNySXf0FcK9Zv
dBgyeUfT3t+r1KR164HOzIc/vSCzpm9bgeCfR097/XFuCqOnoyip9MOGKdcQuvPL
GmjUcb8E3X6qXLZVMETMpGk4aFs6Kq+7zvNedD9sZUTIpjMgrQa4mEIpdfSSgiDE
/U8p/qZWmEv0tJGIvmGqDK0NF3VW4CNTmwqZzMLxnSfL09lcbMZ/kmDKIvYqTMhl
DDGH2wmML4sbbol/cJOucUrvAsQxRj39y+N737ojNUgybgtLiv1FyKzmBx24u0Bb
jfuLKkCfGcw9A8o=
-----END CERTIFICATE-----
|} in
      X509.Encoding.Pem.Certificate.of_pem_cstruct1 data

    let create s ?(port = 8093) ?hostname ?(interval = 10) certificates =
      Metrics.enable_all ();
      let flows = I.create ~interval ?hostname () in
      Lwt.async (L.collect (fun () -> T.sleep_ns (Duration.of_sec interval)));
      let authenticator =
        X509.Authenticator.chain_of_trust
          ~time:(Ptime.v (P.now_d_ps ())) [ca]
      in
      let server = Tls.Config.server ~certificates ~authenticator () in
      S.listen_tcpv4 s ~port (fun flow ->
          TLS.server_of_flow server flow >|= function
          | Ok tls -> I.add_flow flows tls
          | Error e -> Log.err (fun m -> m "TLS error %a" TLS.pp_write_error e))
  end
end

