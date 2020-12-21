
val counter_metrics : f:('a -> string) -> string ->
  (Metrics.field list, 'a -> Metrics.data) Metrics.src

val vmname : string -> Metrics.field
(** [vmname name] creates a [tag] with the virtual machine name. *)

module Make (T : Mirage_time.S) (S : Mirage_stack.V4V6) : sig
  val create : ?interval:int -> ?hostname:string -> Ipaddr.t -> ?port:int -> S.t -> unit
end
