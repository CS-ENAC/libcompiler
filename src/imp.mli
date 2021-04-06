(** Syntaxe du langage source *)

(** Ce module décrit le type des {i abstract syntax trees} (AST) de votre
    compilateur. Les AST seront construits par votre partie frontale
    ({i lexer} et {i parser}, puis transformés en pseudo-assembleur par votre
    partie finale. *)

(** Opérateurs binaires *)
type operator =
  | Plus | Minus | Mult | Div
  | Eq | Lt
  | Or | And

(** Expressions Imp *)
type expression =
  | Num of int (** Nombre entier *)
  | True | False (** Booléens *)
  | Var of string (** Variable *)
  | Not of expression (** Négation booléenne *)
  | Bin of expression * operator * expression (** Opération binaire *)

(** Types des données du langage Imp: entiers ou booléens *)
type typ = Int | Bool

(** Instructions Imp *)
type instruction =
  | Skip (** Ne fait rien *)
  | Expr of expression (** Évaluation d'une expression *)
  | Decl of typ * string (** Déclaration d'une variable avec son type *)
  | Affect of string * expression (** Affectation variable-valeur *)
  | Seq of instruction * instruction (** Séquence de 2 instructions *)
  | Cond of expression * instruction * instruction (** Conditionnelle *)
  | While of expression * instruction (** Boucle non bornée *)

(** Un programme Imp est une instruction *)
type program = instruction


module Operators : sig
  (** Des opérateurs utiles pour rendre le parser plus lisible.
      Par exemple, [a #+ b] est équivalent à [Bin (a, Plus, b)] *)
  val ( + ) : expression -> expression -> expression
  val ( - ) : expression -> expression -> expression
  val ( * ) : expression -> expression -> expression
  val ( / ) : expression -> expression -> expression
  val ( = ) : expression -> expression -> expression
  val ( < ) : expression -> expression -> expression
  val ( > ) : expression -> expression -> expression
  val ( <= ) : expression -> expression -> expression
  val ( >= ) : expression -> expression -> expression
  val ( || ) : expression -> expression -> expression
  val ( && ) : expression -> expression -> expression
  val ( % ) : expression -> expression -> expression
  val ( ~- ) : expression -> expression
end

(** Affiche un programm Imp sur la sortie standard à partir de son AST *)
val print_program : program -> unit
