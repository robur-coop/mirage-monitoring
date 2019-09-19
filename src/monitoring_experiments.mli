
module I : sig
  val encode_line_protocol : Metrics.tags -> Metrics.data -> string -> string
end

module R : sig
  module SM : Map.S with type key = Metrics.Src.t

  val store_reporter : (unit -> int64) -> unit ->
    (unit -> (Metrics.tags * Metrics.data) SM.t) * Metrics.reporter
(** [store_reporter now ()] is a reporter that stores the last measurement from
    each source in a map (which can be retrieved by the returned function).
    This is an initial attempt to overcome the push vs pull interface. Each
    measurement _event_ is sent at an arbitrary point in time, while reporting
    over a communication channel may be rate-limited (i.e. report every 10
    seconds statistics, rather than whenever they appear).

    This is only a good idea for counters, histograms etc. may be useful for
    other numbers (such as time consumed between receive and send - the
    measurement should provide the information whether it's a counter or sth
    else). *)
end

module M : sig
  val vmname : string -> Metrics.field
  (** [vmname name] creates a [tag] with the virtual machine name. *)

  module Pull (T : Mirage_time_lwt.S) (C : Mirage_clock.MCLOCK) (F : Mirage_flow_lwt.S) : sig
    type t
    val create : ?interval:int -> ?hostname:string -> unit -> t
    (** [create_pull ~interval ~hostname ()] registers a reporter, that will
        each [interval] (in seconds, default is 10) send in an asynchronous task
        gathered metrics to all registered flows. Each metrics source produces
        one measurment in influx format. Flows where write fails are
        deregistered. *)

    val add_flow : t -> F.flow -> unit

    val push : ?interval:int -> ?hostname:string -> F.flow -> unit
  end

  module S (T : Mirage_time_lwt.S) (P : Mirage_clock.PCLOCK) (C : Mirage_clock.MCLOCK) (S : Mirage_stack_lwt.V4) : sig
    val create_tls : ?port:int -> ?hostname:string -> ?interval:int -> S.t -> Tls.Config.own_cert -> unit
    val create_tcp : ?port:int -> ?hostname:string -> ?interval:int -> S.t -> unit
  end
end
