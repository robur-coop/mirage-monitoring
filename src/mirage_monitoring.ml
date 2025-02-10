open Lwt.Infix

let src = Logs.Src.create "mirage-monitoring" ~doc:"MirageOS monitoring"
module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Result.bind

let create ~f =
  let data : (string, int) Hashtbl.t = Hashtbl.create 7 in
  (fun x ->
     let key = f x in
     let cur = match Hashtbl.find_opt data key with
       | None -> 0
       | Some x -> x
     in
     Hashtbl.replace data key (succ cur)),
  (fun () ->
     let data, total =
       Hashtbl.fold (fun key value (acc, total) ->
           (Metrics.uint key value :: acc), value + total)
         data ([], 0)
     in
     Metrics.uint "total" total :: data)

let counter_metrics ~f name =
  let open Metrics in
  let doc = "Counter metrics" in
  let incr, get = create ~f in
  let data thing = incr thing; Data.v (get ()) in
  Src.v ~doc ~tags:Metrics.Tags.[] ~data name

let vmname = Metrics.field ~doc:"name of the virtual machine" "vm" Metrics.String

let get_log_levels s =
  let qs = String.split_on_char ',' s in
  let srcs = Logs.Src.list () in
  let srcs = List.map (fun src -> Logs.Src.name src, Logs.Src.level src) srcs in
  let* srcs =
    match qs with
    | [""] ->
      let all_level = Logs.level () in
      Ok (("*", all_level) :: List.filter (fun (_,l) -> l <> all_level) srcs)
    | ["*"] ->
      let all_level = Logs.level () in
      Ok (("*", all_level) :: srcs)
    | qs ->
      let* () =
        let src_names = List.map fst srcs in
        match List.find_opt (fun src -> not (List.mem src src_names)) qs with
        | Some bad_src -> Error ("unknown source: " ^ bad_src)
        | None -> Ok ()
      in
      Ok (List.filter (fun (name, _) -> List.mem name qs) srcs)
  in
  let levels =
    List.map (fun (name, level) ->
        name ^ ":" ^ Logs.level_to_string level)
      srcs
  in
  Ok (`String (String.concat "," levels))

let get_metrics s =
  let qs = String.split_on_char ',' s in
  let srcs = Metrics.Src.list () in
  let srcs =
    List.map (fun src ->
        Metrics.Src.name src, Metrics.Src.is_active src)
      srcs
  in
  let* srcs =
    match qs with
    | [""] ->
      let all = Metrics.all_enabled () in
      Ok (("*", all) :: (List.filter (fun (_, b) -> b <> all) srcs))
    | ["*"] ->
      let all = Metrics.all_enabled () in
      let tags = Metrics.tags_enabled () in
      Ok (("*", all) :: List.map (fun t -> "tag:" ^ t, true) tags @ srcs)
    | qs ->
      let* () =
        let src_names = List.map fst srcs in
        match List.find_opt (fun src -> not (List.mem src src_names)) qs with
        | Some bad_src -> Error ("unknown source: " ^ bad_src)
        | None -> Ok ()
      in
      Ok (List.filter (fun (n, _) -> List.mem n qs) srcs)
  in
  let metrics =
    List.map (fun (name, act) ->
        name ^ ":" ^ if act then "enabled" else "disabled")
      srcs
  in
  Ok (`String (String.concat "," metrics))

let adjust_log_level s =
  let ts =
    List.map
      (fun s -> (fst Mirage_runtime.Conv.log_threshold) s)
      (String.split_on_char ',' s)
  in
  let* oks =
    List.fold_left (fun acc t ->
        let* acc = acc in
        match t with
        | `Ok l -> Ok (l :: acc)
        | `Error msg -> Error msg)
      (Ok []) ts
  in
  Mirage_runtime.set_level ~default:(Logs.level ()) oks;
  Ok `Empty

let enable_of_str s =
  let s = String.lowercase_ascii s in
  if s = "enable" || s = "on" then
    Ok `Enable
  else if s = "disable" || s = "off" then
    Ok `Disable
  else
    Error ("couldn't decode 'enable' or 'disable': " ^ s)

let adjust_metrics s =
  let ts =
    List.map (fun s ->
        match String.split_on_char ':' s with
        | [ en ] | [ "*" ; en ] ->
          let* en_or_d = enable_of_str en in
          Ok (`All, en_or_d)
        | [ src ; en ] ->
          let* en_or_d = enable_of_str en in
          Ok (`Src src, en_or_d)
        | [ "src" ; src ; en ] ->
          let* en_or_d = enable_of_str en in
          Ok (`Src src, en_or_d)
        | [ "tag" ; tag ; en ] ->
          let* en_or_d = enable_of_str en in
          Ok (`Tag tag, en_or_d)
        | _ -> Error ("couldn't decode metrics " ^ s))
      (String.split_on_char ',' s)
  in
  let* (all, srcs, tags) =
    List.fold_left (fun acc t ->
        let* (all, srcs, tags) = acc in
        let* t = t in
        match t with
        | `All, en_or_d -> Ok (Some en_or_d, srcs, tags)
        | `Src s, en_or_d -> Ok (all, (s, en_or_d) :: srcs, tags)
        | `Tag t, en_or_d -> Ok (all, srcs, (t, en_or_d) :: tags))
      (Ok (None, [], [])) ts
  in
  (match all with
   | Some `Enable -> Metrics.enable_all ()
   | Some `Disable -> Metrics.disable_all ()
   | None -> ());
  List.iter (fun (tag, e_or_d) ->
      match e_or_d with
      | `Enable -> Metrics.enable_tag tag
      | `Disable -> Metrics.disable_tag tag)
    tags ;
  List.iter (fun (src, e_or_d) ->
      match List.find_opt (fun s -> Metrics.Src.name s = src) (Metrics.Src.list ()), e_or_d with
      | Some src, `Enable -> Metrics.Src.enable src
      | Some src, `Disable -> Metrics.Src.disable src
      | None, _ ->
        Log.warn (fun m -> m "%s is not a valid metrics source." src))
    srcs ;
  Ok `Empty

module Make (S : Tcpip.Stack.V4V6) = struct
  module Memtrace = Memtrace.Make(S.TCP)

  let timer conn get host stack dst =
    let datas =
      Metrics.SM.fold (fun src (tags, data) acc ->
          let name = Metrics.Src.name src in
          Metrics_influx.encode_line_protocol (host@tags) data name :: acc)
        (get ()) []
    in
    let datas = String.concat "" datas in
    let write flow =
      Log.debug (fun m -> m "sending measurements");
      S.TCP.write flow (Cstruct.of_string datas) >|= function
      | Ok () -> ()
      | Error e ->
        Log.err (fun m -> m "error %a writing to metrics" S.TCP.pp_write_error e);
        conn := None
    in
    match !conn with
    | None ->
      begin
        Log.debug (fun m -> m "creating connection");
        S.TCP.create_connection (S.tcp stack) dst >>= function
        | Error msg ->
          Log.err (fun m -> m "couldn't create connection %a"
                      S.TCP.pp_error msg);
          Lwt.return_unit
        | Ok flow ->
          conn := Some flow;
          write flow
      end
    | Some f -> write f

  let timer_loop get host interval stack dst () =
    let conn = ref None in
    let rec one () =
      Lwt.join [
          timer conn get host stack dst;
          Mirage_sleep.ns (Duration.of_sec interval)
        ] >>= fun () ->
      (one[@tailcall]) ()
    in
    one ()

  let create_listener stack port =
    S.TCP.listen (S.tcp stack) ~port (fun f ->
        (S.TCP.read f >>= function
          | Ok `Data data ->
            if Cstruct.length data > 0 then
              let rest = Cstruct.(to_string (shift data 1)) in
              let r =
                match Cstruct.get_char data 0 with
                | 'L' -> adjust_log_level rest
                | 'M' -> adjust_metrics rest
                | 'l' -> get_log_levels rest
                | 'm' -> get_metrics rest
                | _ -> Error "unknown command"
              in
              let msg =
                match r with
                | Ok `Empty -> "ok"
                | Ok `String reply -> "ok: " ^ reply
                | Error msg -> "error: " ^ msg
              in
              S.TCP.write f (Cstruct.of_string msg) >|= function
              | Ok () -> ()
              | Error e ->
                Log.warn (fun m -> m "write error on log & metrics listener %a"
                             S.TCP.pp_write_error e)
            else
              (Log.debug (fun m -> m "received empty data on log & metrics listener");
               Lwt.return_unit)
          | Ok `Eof ->
            Log.debug (fun m -> m "EOF on log & metrics listener");
            Lwt.return_unit
          | Error e ->
            Log.debug (fun m -> m "read error on log & metrics listener %a"
                          S.TCP.pp_error e);
            Lwt.return_unit) >>= fun () ->
        S.TCP.close f)

  let create ?(interval = 10) ?hostname dst ?(port = 8094) ?(listen_port = 2323)
      ?(memtrace_port = 4242) ?(sampling_rate = 1e-4) stack =
    S.TCP.listen (S.tcp stack) ~port:memtrace_port
      (fun f ->
         (* only allow a single tracing client *)
       match Memtrace.Memprof_tracer.active_tracer () with
       | Some _ ->
           Log.warn (fun m -> m "memtrace tracing already active");
           S.TCP.close f
       | None ->
           Logs.info (fun m -> m "starting memtrace tracing");
           let tracer = Memtrace.start_tracing ~context:None ~sampling_rate f in
           Lwt.async (fun () ->
               S.TCP.read f >|= fun _ ->
               Logs.info (fun m -> m "memtrace tracing read returned, closing");
               Memtrace.stop_tracing tracer);
           Lwt.return_unit);
    let get_cache, reporter = Metrics.cache_reporter () in
    Metrics.set_reporter reporter;
    Metrics.enable_all ();
    Metrics_lwt.init_periodic (fun () -> Mirage_sleep.ns (Duration.of_sec interval));
    let host = match hostname with None -> [] | Some host -> [vmname host] in
    Lwt.async (timer_loop get_cache host interval stack (dst, port));
    create_listener stack listen_port
end

