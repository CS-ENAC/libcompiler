(** Langage pseudo-assembleur ASM *)

(** Ce module décrit le langage pseudo-assembleur (ASM) que doit produire votre
    compilateur. Il fournit également des fonctions qui vous faciliteront
    la gestion de la mémoire et des branchements. *)

(** {2 Registres} *)
(** Le processeur simulé contient 4 registres : ax bx cx et dx *)

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

type instruction

(** [mov r1 r2]: [r1] <- [r2] *)
val mov : register -> register -> instruction

(** [imov n r]: la constante [n] est placée dans le registre [r] *)
val imov : int -> register -> instruction

(** [add r1 r2 r3]: [r1] <- [r2 + r3] *)
val add : register -> register -> register -> instruction

(** [sub r1 r2 r3]: [r1] <- [r2 - r3] *)
val sub : register -> register -> register -> instruction

(** [mul r1 r2 r3]: [r1] <- [r2 * r3] *)
val mul : register -> register -> register -> instruction

(** [div r1 r2 r3]: [r1] <- [r2 / r3] *)
val div : register -> register -> register -> instruction

(** [slt r1 r2 r3]: [r1] <- [r2 < r3] *)
val slt : register -> register -> register -> instruction

(** [seq r1 r2 r3]: [r1] <- [r2 = r3] *)
val seq : register -> register -> register -> instruction

(** Négation booléenne du registre [r] *)
val neg : register -> instruction

(** [lda a r]: le contenu de l'adresse [a] est placé dans le registre [r] *)
val lda : Memory.address -> register -> instruction

(** [sta a r]: le contenu du registre [r] est stocké à l'adresse [a] *)
val sta : Memory.address -> register -> instruction

(** [Jmp a]: saute à l'adresse [a] dans le code *)
val jmp : label -> instruction

(** [brz a r] saute à l'adresse [a] si le contenu du registre [r] vaut 0 *)
val brz : label -> register -> instruction

(** [push r] empile le contenu du registre [r] *)
val push : register -> instruction

(** [pop r] dépile le sommet de pile dans le registre [r] *)
val pop : register -> instruction

(** [call a] saute à l'adresse [a] en empilant l'adresse de retour *)
val call : Memory.address -> instruction

(** [ret] revient à l'adresse de retour *)
val ret : instruction

(**/**)
val inp : register -> instruction
val out : register -> instruction
(**/**)

(** Arrêt du processus *)
val hlt : instruction

(** Pseudo-instruction permettant de placer une étiquette dans le code *)
val lbl : label -> instruction

(** Le type des programmes ASM *)
type program = instruction list

(** Exécute le programme *)
val exec : ?fast:bool -> program -> unit

(**/**)
val output_asm : string -> program -> unit
