type t =
  | AX | BX | CX | DX (** user registers *)
  | SP | PC | IR | CC | IN | OUT (** reserved registers *)

val size_bytes : t -> int
val size : t -> int

type value_type = private Addr | Instr | Unsigned | Signed
val value_type : t -> value_type

val to_string : t -> string
val fprint : Format.formatter -> t -> unit

val next : t -> t

type file
val file : unit -> file
val get : file -> t -> int
val set : file -> t -> int -> file
val iteri : (int -> int -> unit) -> file -> unit

module Operators : sig
  val ( !> ) : t -> int
  val ( !< ) : int -> t
end
