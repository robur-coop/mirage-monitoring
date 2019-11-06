open Lwt.Infix

let src = Logs.Src.create "influx" ~doc:"influx metrics reporter"
module Log = (val Logs.src_log src : Logs.LOG)

let vmname = Metrics.field ~doc:"name of the virtual machine" "vm" Metrics.String

module Make (T : Mirage_time.S) (S : Mirage_stack.V4) = struct

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
      S.TCPV4.write flow (Cstruct.of_string datas) >|= function
      | Ok () -> ()
      | Error e ->
        Log.err (fun m -> m "error %a writing to metrics" S.TCPV4.pp_write_error e);
        conn := None
    in
    match !conn with
    | None ->
      begin
        Log.debug (fun m -> m "creating connection");
        S.TCPV4.create_connection (S.tcpv4 stack) dst >>= function
        | Error msg ->
          Log.err (fun m -> m "couldn't create connection %a"
                      S.TCPV4.pp_error msg);
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

  let create ?(interval = 10) ?hostname stack dst =
    let get_cache, reporter = Metrics.cache_reporter () in
    Metrics.set_reporter reporter;
    Metrics.enable_all ();
    Metrics_lwt.init_periodic (fun () -> T.sleep_ns (Duration.of_sec interval));
    Metrics_lwt.periodically (OS.MM.malloc_metrics ~tags:Metrics.Tags.[]);
    let host = match hostname with None -> [] | Some host -> [vmname host] in
    Lwt.async (timer_loop get_cache host interval stack dst)
end

