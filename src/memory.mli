(** Gestion de la mémoire et navigation dans le code *)

(** Chaque donnée ou instruction est repérée par une adresse. *)
type address

(** Il est possible de stocker des valeurs dans la mémoire associée au
    processeur. Il faut pour cela allouer une case mémoire, puis l'associer
    (dans une table des symboles) à un nom de variable dans la phase de
    compilation. *)

(** Obtenir une adresse mémoire pour stocker une nouvelle variable *)
val new_address : string -> address

exception Unbound of string

(** Récupérer l'adresse mémoire d'une variable déjà déclarée. Lève l'exception
    [Unbound x] si la variable [x] n'a pas été correctement déclarée. *)
val get_address : string -> address

(**/**)
module Operators : sig
  val ( +@ ) : address -> int -> address
end

val sprint_addr : address -> string
val fprint_addr : Format.formatter -> address -> unit
val addr_to_int : address -> int
val addr_of_int : int -> address

val block_size_bytes : int
val block_size : int
val sprint_data : int -> string

type ram
type t = { memory : ram;
           size_bytes : int;
           code_start : address;
           data_start : address;
           heap_start : address;
           free_start : address }
val ram : unit -> t

val fetch : t -> address -> int -> int
val store : t -> address -> int -> int -> t
val load_program : t -> int array -> int -> t
val get : t -> address -> int
