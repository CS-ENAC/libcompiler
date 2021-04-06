type t = | AX | BX | CX | DX
         | SP | PC | IR | CC | IN | OUT
external to_int : t -> int = "%identity"
external of_int : int -> t = "%identity"

let nb_regs = 10
type file = int array
let file = fun () -> Array.make nb_regs 0
let get = fun f r -> f.(to_int r)
let set = fun f r v -> let f' = Array.copy f in f'.(to_int r) <- v; f'

let iteri = Array.iteri

let size_bytes = function PC | SP -> 1 | _ -> 2
let size = fun r -> 2 * size_bytes r

type value_type = Addr | Instr | Unsigned | Signed
let value_type = function
  | PC | SP -> Addr
  | IR -> Instr
  | CC -> Unsigned
  | _ -> Signed

let to_string = function
  | AX -> "%ax" | BX -> "%bx" | CX -> "%cx" | DX -> "%dx"
  | SP -> "%sp" | PC -> "%pc" | IR -> "%ir" | CC -> "%cc"
  | IN -> "%in" | OUT -> "%out"

let fprint = fun ch r -> Format.fprintf ch "%s" (to_string r)

exception Overflow
let next = function
  | AX -> BX | BX -> CX | CX -> DX | DX -> raise Overflow
  | _ -> invalid_arg "Asm.Reg.next called on reserved register"

module Operators = struct
  let ( !> ) = to_int
  let ( !< ) = of_int
end
