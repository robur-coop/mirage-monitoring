open Lwt.Infix

let src = Logs.Src.create "monitoring-experiments" ~doc:"Monitoring experiments"
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

let memory_metrics ~tags =
  let open Metrics in
  let doc = "Memory counters" in
  let data () =
    let stat = OS.Memory.quick_stat () in
    Data.v
      [ uint "memory heap words" stat.heap_words
      ; uint "memory live words" stat.live_words
      ; uint "memory stack words" stat.stack_words
      ; uint "memory free words" stat.free_words ]
  in
  Src.v ~doc ~tags ~data "memory"

let adjust_log_level s =
  let ts =
    List.map
      (fst Mirage_runtime.Arg.log_threshold)
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
  Ok (Mirage_runtime.set_level
        ~default:(Option.value (Logs.level ()) ~default:Logs.Info)
        oks)

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
        | [ "tag" ; src ; en ] ->
          let* en_or_d = enable_of_str en in
          Ok (`Tag src, en_or_d)
        | _ -> Error ("couldn't decode metrics " ^ s))
      (String.split_on_char ',' s)
  in
  let* (all, tags, srcs) =
    List.fold_left (fun acc t ->
        let* (all, tags, srcs) = acc in
        let* t = t in
        match t with
        | `All, en_or_d -> Ok (Some en_or_d, tags, srcs)
        | `Src s, en_or_d -> Ok (all, tags, (s, en_or_d) :: srcs)
        | `Tag t, en_or_d -> Ok (all, (t, en_or_d) :: tags, srcs))
      (Ok (None, [], [])) ts
  in
  (match all with
   | Some `Enable -> Metrics.enable_all ()
   | Some `Disable -> Metrics.disable_all ()
   | None -> ());
  List.iter
    (function
      | t, `Enable -> Metrics.enable_tag t
      | t, `Disable -> Metrics.disable_tag t)
    tags ;
  List.iter (fun (src, e_or_d) ->
      match List.find_opt (fun s -> Metrics.Src.name s = src) (Metrics.Src.list ()), e_or_d with
      | Some src, `Enable -> Metrics.Src.enable src
      | Some src, `Disable -> Metrics.Src.disable src
      | None, _ ->
        Log.warn (fun m -> m "%s is not a valid metrics source." src))
    srcs ;
  Ok ()

module Make (T : Mirage_time.S) (S : Tcpip.Stack.V4V6) = struct

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
          T.sleep_ns (Duration.of_sec interval)
        ] >>= fun () ->
      (one[@tailcall]) ()
    in
    one ()

  let create_listener stack = function
    | None -> ()
    | Some port ->
      S.TCP.listen (S.tcp stack) ~port (fun f ->
          (S.TCP.read f >>= function
            | Ok `Data data ->
              if Cstruct.length data > 0 then
                let rest = Cstruct.(to_string (shift data 1)) in
                let r =
                  match Cstruct.get_char data 0 with
                  | 'L' -> adjust_log_level rest
                  | 'M' -> adjust_metrics rest
                  | _ -> Error "unknown command"
                in
                let msg =
                  match r with
                  | Ok () -> "ok" | Error msg -> "error: " ^ msg
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

  let create ?(interval = 10) ?hostname dst ?(port = 8094) ?listen_port stack =
    let get_cache, reporter = Metrics.cache_reporter () in
    Metrics.set_reporter reporter;
    Metrics.enable_all ();
    Metrics_lwt.init_periodic (fun () -> T.sleep_ns (Duration.of_sec interval));
    Metrics_lwt.periodically (OS.MM.malloc_metrics ~tags:Metrics.Tags.[])[@warning "-3"];
    Metrics_lwt.periodically (memory_metrics ~tags:Metrics.Tags.[]);
    let host = match hostname with None -> [] | Some host -> [vmname host] in
    Lwt.async (timer_loop get_cache host interval stack (dst, port));
    create_listener stack listen_port
end

