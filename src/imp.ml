type operator =
  | Plus | Minus | Mult | Div
  | Eq | Lt
  | Or | And

type typ = Int | Bool

type expression =
  | Num of int
  | True | False
  | Var of string
  | Not of expression
  | Bin of expression * operator * expression

module Operators = struct
  let ( + ) = fun a b -> Bin (a, Plus, b)
  let ( - ) = fun a b -> Bin (a, Minus, b)
  let ( * ) = fun a b -> Bin (a, Mult, b)
  let ( / ) = fun a b -> Bin (a, Div, b)
  let ( = ) = fun a b -> Bin (a, Eq, b)
  let ( < ) = fun a b -> Bin (a, Lt, b)
  let ( > ) = fun a b -> Bin (b, Lt, a)
  let ( || ) = fun a b -> Bin (a, Or, b)
  let ( && ) = fun a b -> Bin (a, And, b)
  let ( % ) = fun a b -> a - (b * (a / b))
  let ( ~- ) = fun a -> Num 0 - a
  let ( <= ) = fun a b -> a < b || a = b
  let ( >= ) = fun a b -> a > b || a = b
end

type instruction =
  | Skip
  | Expr of expression
  | Decl of typ * string
  | Affect of string * expression
  | Seq of instruction * instruction
  | Cond of expression * instruction * instruction
  | While of expression * instruction

type program = instruction

let fprint_typ ch = function
  | Int -> Format.fprintf ch "int"
  | Bool -> Format.fprintf ch "bool"

let fprint_op = fun ch op ->
  Format.fprintf ch "%s"
    (match op with
     | Plus -> "+" | Minus -> "-" | Mult -> "*" | Div -> "/"
     | Eq -> "=" | Lt -> "<"
     | Or -> "|" | And -> "&")

let rec fprint_expr ch = function
  | Num n -> Format.fprintf ch "%d" n
  | True -> Format.fprintf ch "True" | False -> Format.fprintf ch "False"
  | Var v -> Format.fprintf ch "%s" v
  | Not e -> Format.fprintf ch "@[not@ @[(%a)@]@]" fprint_expr e
  | Bin (e1, op, e2) ->
     Format.fprintf ch "@[(%a@ %a@ %a)@]"
       fprint_expr e1 fprint_op op fprint_expr e2

let rec fprint_instr ch = function
  | Skip -> Format.fprintf ch "skip"
  | Expr e -> Format.fprintf ch "@[%a@]" fprint_expr e
  | Decl (t, v) -> Format.fprintf ch "@[%a %s@]" fprint_typ t v
  | Affect (id, e) -> Format.fprintf ch "@[%s :=@ %a@]" id fprint_expr e
  | Seq (i1, i2) ->
     Format.fprintf ch "@[<v 0>%a;;@,%a@]" fprint_instr i1 fprint_instr i2
  | Cond (c, i1, i2) ->
     Format.fprintf ch "@[<v 0>if %a then@;<1 2>%a@,else@;<1 2>%a@]"
       fprint_expr c fprint_instr i1 fprint_instr i2
  | While (c, i) ->
     Format.fprintf ch "@[<v 0>while %a do@;<1 2>%a@,end@]"
       fprint_expr c fprint_instr i

let print_program = fun p ->
  Format.printf "@,Code reconnu:@,@[%a@]@." fprint_instr p

(* let rec expr2dot ch = function
 *   | Num n -> Format.fprintf ch "E -- %d;@," n
 *   | True -> Format.fprintf ch "E -- True;@,"
 *   | False -> Format.fprintf ch "E -- False;@,"
 *   | Var v -> Format.fprintf ch "E -- %s;@," v
 *   | Not e -> Format.fprintf ch "@[E -- {not @[%a@]}@]@," expr2dot e
 *   | Bin (e1, op, e2) ->
 *      Format.fprintf ch "@[E -- {%a %a %a}@]@,"
 *        expr2dot e1 fprint_op op expr2dot e2 *)
