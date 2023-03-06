(** Langage pseudo-assembleur ASM *)

(** Ce module décrit le langage pseudo-assembleur (ASM) que doit produire votre
    compilateur. Il fournit également des fonctions qui vous faciliteront
    la gestion de la mémoire et des branchements. *)

(** {2 Registres} *)
(** Le processeur simulé contient 4 registres : [ax] [bx] [cx] et [dx] *)

type register

val ax : register
val bx : register
val cx : register
val dx : register
val nb_registers : int

(** [next_reg r] renvoie le registre qui suit [r]. Valide uniquement pour les
    registres non réservés. *)
val next_reg: register -> register

(** {2 Langage ASM} *)

(** {3 Étiquettes} *)

(** Pour effectuer des branchements, vous aurez besoin d'étiquettes,
    que vous pouvez créer avec la fonction suivante. *)
type label
val create_label : string -> label

(** {3 Les instructions} *)

type asm

(** Une instruction qui ne fait rien *)
val noop : asm

(** [mov r1 r2]: [r1] <- [r2] *)
val mov : register -> register -> asm

(** [imov n r]: la constante [n] est placée dans le registre [r] *)
val imov : int -> register -> asm

(** [add r1 r2 r3]: [r1] <- [r2 + r3] *)
val add : register -> register -> register -> asm

(** [sub r1 r2 r3]: [r1] <- [r2 - r3] *)
val sub : register -> register -> register -> asm

(** [mul r1 r2 r3]: [r1] <- [r2 * r3] *)
val mul : register -> register -> register -> asm

(** [div r1 r2 r3]: [r1] <- [r2 / r3] *)
val div : register -> register -> register -> asm

(** [slt r1 r2 r3]: [r1] <- [r2 < r3] *)
val slt : register -> register -> register -> asm

(** [seq r1 r2 r3]: [r1] <- [r2 = r3] *)
val seq : register -> register -> register -> asm

(** Négation booléenne du registre [r] *)
val neg : register -> asm

(** [lda a r]: le contenu de l'adresse [a] est placé dans le registre [r] *)
val lda : Memory.address -> register -> asm

(** [sta a r]: le contenu du registre [r] est stocké à l'adresse [a] *)
val sta : Memory.address -> register -> asm

(** [jmp a]: saute à l'adresse [a] dans le code *)
val jmp : label -> asm

(** [brz a r] saute à l'adresse [a] si le contenu du registre [r] vaut 0 *)
val brz : label -> register -> asm

(** [push r] empile le contenu du registre [r] *)
val push : register -> asm

(** [pop r] dépile le sommet de pile dans le registre [r] *)
val pop : register -> asm

(**/**)
(** [call a] saute à l'adresse [a] en empilant l'adresse de retour *)
val call : Memory.address -> asm

(** [ret] revient à l'adresse de retour *)
val ret : asm

val inp : register -> asm
val out : register -> asm
(**/**)

(** Arrêt du processus *)
val hlt : asm

(** Pseudo-instruction permettant de placer une étiquette dans le code *)
val lbl : label -> asm

(** Le type des programmes ASM *)
type program = asm

(** Concatène des instructions *)
val ( & ) : asm -> asm -> asm

(** Exécute le programme *)
val exec : ?fast:bool -> program -> unit

(**/**)
val output_asm : string -> program -> unit
