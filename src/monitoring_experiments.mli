val vmname : string -> Metrics.field
(** [vmname name] creates a [tag] with the virtual machine name. *)

module Make (T : Mirage_time.S) (S : Mirage_stack.V4) : sig
  val create : ?interval:int -> ?hostname:string -> Ipaddr.V4.t -> ?port:int -> S.t -> unit
end
