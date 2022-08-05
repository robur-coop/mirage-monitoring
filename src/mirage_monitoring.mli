
val counter_metrics : f:('a -> string) -> string ->
  (Metrics.field list, 'a -> Metrics.data) Metrics.src

val vmname : string -> Metrics.field
(** [vmname name] creates a [tag] with the virtual machine name. *)

module Make (T : Mirage_time.S) (P : Mirage_clock.PCLOCK) (S : Tcpip.Stack.V4V6) : sig

  val create : ?interval:int -> ?quick:bool -> ?hostname:string -> Ipaddr.t ->
    ?port:int -> ?listen_port:int -> ?memtrace_port:int ->
    ?sampling_rate:float -> S.t -> unit
  (** [create ~interval ~quick ~hostname ip ~port ~listen_port ~memtrace_port ~sampling_rate stack]
      initiates monitoring on [stack] for the unikernel. The metrics are reported
      every [interval] (defaults to 10) seconds to [ip] on [port] (defaults to
      8094) via TCP using the influxd wire protocol. On [listen_port] (defaults
      to 2323) a TCP connection can be initiated to adjust the log level and
      enable and disable metrics sources. On [memtrace_port] (defaults to 4242)
      a single TCP client can connect simultaneously to receive a [Gc.Memprof]
      trace. The [sampling_rate] defaults to [1e-4]. If [quick] is provided
      and [false] (defaults to [true]), [Solo5_os.Memory.stat] is used,
      otherwise [Solo5_os.Memory.quick_stat] is used. *)
end
