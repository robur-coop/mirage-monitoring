open Lwt.Infix

let src = Logs.Src.create "monitoring-experiments" ~doc:"Monitoring experiments"
module Log = (val Logs.src_log src : Logs.LOG)

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

module Make (T : Mirage_time.S) (S : Mirage_stack.V4V6) = struct

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

  let create ?(interval = 10) ?hostname dst ?(port = 8094) stack =
    let get_cache, reporter = Metrics.cache_reporter () in
    Metrics.set_reporter reporter;
    Metrics.enable_all ();
    Metrics_lwt.init_periodic (fun () -> T.sleep_ns (Duration.of_sec interval));
    Metrics_lwt.periodically (OS.MM.malloc_metrics ~tags:Metrics.Tags.[]);
    let host = match hostname with None -> [] | Some host -> [vmname host] in
    Lwt.async (timer_loop get_cache host interval stack (dst, port))
end

