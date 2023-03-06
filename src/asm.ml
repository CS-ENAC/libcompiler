open Values
open Memory.Operators
open Register.Operators

module M = Memory
module R = Register

type register = R.t
let ax = R.AX and bx = R.BX and cx = R.CX and dx = R.DX
let next_reg = R.next
let nb_registers = 4

type label = string
let create_label =
  let labels = Hashtbl.create 17 in
  let uniq = let n = ref ~-1 in fun s -> incr n; Printf.sprintf "%s%d" s !n in
  fun s -> let s = if Hashtbl.mem labels s then uniq s else s in
           Hashtbl.add labels s (); s

type instruction =
  | Mov of register * register
  | IMov of int * register
  | Add of register * register * register
  | Sub of register * register * register
  | Mul of register * register * register
  | Div of register * register * register
  | Slt of register * register * register
  | Seq of register * register * register
  | Neg of register
  | Lda of M.address * register
  | Sta of M.address * register
  | LblJmp of label
  | LblBrz of label * register
  | Jmp of M.address
  | Brz of M.address * register
  | Push of register
  | Pop of register
  | Call of M.address
  | Ret
  | In of register
  | Out of register
  | Hlt
  | Lbl of label
  | Invalid

let fprint_bin = fun ch instr r1 r2 r3 ->
  Format.fprintf ch "%s   %a, %a, %a" instr R.fprint r1 R.fprint r2 R.fprint r3

let fprint_instr ch =
  let open Format in
  function
  | Mov (r1, r2) -> fprintf ch "mov   %a, %a" R.fprint r1 R.fprint r2
  | IMov (n, r) -> fprintf ch "imov  %d, %a" n R.fprint r
  | Add (r1, r2, r3) -> fprint_bin ch "add" r1 r2 r3
  | Sub (r1, r2, r3) -> fprint_bin ch "sub" r1 r2 r3
  | Mul (r1, r2, r3) -> fprint_bin ch "mul" r1 r2 r3
  | Div (r1, r2, r3) -> fprint_bin ch "div" r1 r2 r3
  | Slt (r1, r2, r3) -> fprint_bin ch "slt" r1 r2 r3
  | Seq (r1, r2, r3) -> fprint_bin ch "seq" r1 r2 r3
  | Neg r -> fprintf ch "neg   %a" R.fprint r
  | Lda (a, r) -> fprintf ch "lda   %a, %a" M.fprint_addr a R.fprint r
  | Sta (a, r) -> fprintf ch "sta   %a, %a" M.fprint_addr a R.fprint r
  | LblJmp l -> fprintf ch "jmp   %s" l
  | LblBrz (l, r) -> fprintf ch "brz   %s, %a" l R.fprint r
  | Jmp a -> fprintf ch "jmp   %a" M.fprint_addr a
  | Brz (a, r) -> fprintf ch "brz   %a, %a" M.fprint_addr a R.fprint r
  | Push r -> fprintf ch "push  %a" R.fprint r
  | Pop r -> fprintf ch "pop   %a" R.fprint r
  | Call a -> fprintf ch "call  %a" M.fprint_addr a
  | Ret -> fprintf ch "ret"
  | In r -> fprintf ch "in    %a" R.fprint r
  | Out r -> fprintf ch "out   %a" R.fprint r
  | Hlt -> fprintf ch "hlt"
  | Lbl s -> fprintf ch "%s" s
  | Invalid -> fprintf ch "###########"

let sprint_instr = Format.asprintf "%a" fprint_instr

let instr_size_bytes = 2
let nb_instr_chunks = instr_size_bytes / M.block_size_bytes

let rec nb_instr = function
  | [] -> 0
  | Lbl _ :: prog -> nb_instr prog
  | _ :: prog -> 1 + nb_instr prog

let remap_data_addr = fun program ->
  let n = nb_instr_chunks * nb_instr program in
  List.map (function
      | Lda (a, r) -> Lda (a +@ n, r)
      | Sta (a, r) -> Sta (a +@ n, r)
      | i -> i) program

let rec compact_labels = function
  | [] -> []
  | [i] -> [i]
  | Lbl s1 :: Lbl s2 :: is -> compact_labels (Lbl (s1 ^ "_" ^ s2) :: is)
  | i :: is -> i :: compact_labels is

let fprint_asm = fun ch program ->
  let open Format in
  let program = program |> remap_data_addr |> compact_labels in
  let label_width =
    3 + List.fold_left (fun n instr ->
            match instr with Lbl s -> max n (String.length s) | _ -> n
          ) 0 program in
  let rec dump ch = function
    | [] -> ()
    | [Lbl _] -> ()
    | Lbl s :: i :: prog ->
       let fill = label_width - String.length s - 3 in
       fprintf ch "%s:  %*s%a@,%a" s fill "" fprint_instr i dump prog
    | i :: prog -> fprintf ch "%*s%a@,%a" label_width "" fprint_instr i dump prog
  in fprintf ch "@[<v>%a@]@." dump program

let output_asm = fun file program ->
  let ch = open_out file in
  fprint_asm (Format.formatter_of_out_channel ch) program;
  close_out ch

type asm = instruction list
let list = fun x -> [x]

let noop = []
let mov = fun r1 r2 -> Mov (r1, r2) |> list
let imov = fun n r -> IMov (n, r) |> list
let add = fun r1 r2 r3 -> Add (r1, r2, r3) |> list
let sub = fun r1 r2 r3 -> Sub (r1, r2, r3) |> list
let mul = fun r1 r2 r3 -> Mul (r1, r2, r3) |> list
let div = fun r1 r2 r3 -> Div (r1, r2, r3) |> list
let slt = fun r1 r2 r3 -> Slt (r1, r2, r3) |> list
let seq = fun r1 r2 r3 -> Seq (r1, r2, r3) |> list
let neg = fun r -> Neg r |> list
let lda = fun a r -> Lda (a, r) |> list
let sta = fun a r -> Sta (a, r) |> list
let jmp = fun lbl -> LblJmp lbl |> list
let brz = fun lbl r -> LblBrz (lbl, r) |> list
let push = fun r -> Push r |> list
let pop = fun r -> Pop r |> list
let call = fun a -> Call a |> list
let ret = Ret |> list
let inp = fun r -> In r |> list
let out = fun r -> Out r |> list
let hlt = Hlt |> list
let lbl = fun l -> Lbl l |> list

type program = asm

let ( & ) = ( @ )

(* let rec remove_dup = function
 *   | [] -> []
 *   | [x] -> [x]
 *   | x1 :: ((x2 :: _) as xs) ->
 *      if x1 = x2 then remove_dup xs else x1 :: remove_dup xs
 *
 * let heads = fun prog ->
 *   let rec iter i acc = function
 *     | [] -> remove_dup (List.sort compare acc)
 *     | Lbl _ :: is -> iter (i + 1) (i :: acc) is
 *     | LblJmp _ :: is | LblBrz _ :: is | Jmp _ :: is | Brz _ :: is ->
 *        iter (i + 1) ((i + 1) :: acc) is
 *     | _ :: is -> iter (i + 1) acc is in
 *   iter 0 [0] prog
 *
 * let optimize = fun program ->
 *   let
 *   let aprogram = Array.of_list program in *)

let compute_addresses = fun program ->
  let rec iter addrs addr = function
    | [] -> addrs
    | Lbl lbl :: is -> iter ((lbl, addr) :: addrs) addr is
    | _ :: is -> iter addrs (addr +@ nb_instr_chunks) is
  in iter [] (M.addr_of_int 0) program

let compute_branching = fun addresses program ->
  List.map (function
      | LblJmp lbl -> Jmp (List.assoc lbl addresses)
      | LblBrz (lbl, r) -> Brz (List.assoc lbl addresses, r)
      | instr -> instr) program

let remove_labels = List.filter (function Lbl _ -> false | _ -> true)

let make_program = fun program ->
  program
  |> remap_data_addr
  |> remove_labels
  |> compute_branching (compute_addresses program)
  |> Array.of_list

let enc = fun opcode x y z -> opcode lsl 11 + x lsl 6 + y lsl 3 + z

let encode = function
  | Mov (r1, r2) -> enc 0x00 0 !>r1 !>r2
  | IMov (n, r) -> enc 0x01 0 (Int8.encode n) !>r
  | Add (r1, r2, r3) -> enc 0x02 !>r1 !>r2 !>r3
  | Sub (r1, r2, r3) -> enc 0x03 !>r1 !>r2 !>r3
  | Mul (r1, r2, r3) -> enc 0x04 !>r1 !>r2 !>r3
  | Div (r1, r2, r3) -> enc 0x05 !>r1 !>r2 !>r3
  | Slt (r1, r2, r3) -> enc 0x07 !>r1 !>r2 !>r3
  | Seq (r1, r2, r3) -> enc 0x09 !>r1 !>r2 !>r3
  | Neg r -> enc 0x0D 0 0 !>r
  (* Possible instructions with op codes 0x0E -> 0x0F *)
  | Lda (a, r) -> enc 0x10 0 (M.addr_to_int a) !>r
  | Sta (a, r) -> enc 0x11 0 (M.addr_to_int a) !>r
  | Jmp a -> enc 0x12 0 (M.addr_to_int a) 0
  | Brz (a, r) -> enc 0x13 0 (M.addr_to_int a) !>r
  | Push r -> enc 0x14 0 0 !>r
  | Pop r -> enc 0x15 0 0 !>r
  | Call a -> enc 0x16 0 (M.addr_to_int a) 0
  | Ret -> enc 0x17 0 0 0
  (* Possible instructions with op codes 0x18 -> 0x1B *)
  | In r -> enc 0x1C 0 0 !>r
  | Out r -> enc 0x1D 0 0 !>r
  | Hlt -> enc 0x1F 0 0 0
  | Lbl _ | LblJmp _ | LblBrz _ -> failwith "Asm.encode: remove labels first"
  | Invalid -> failwith "Asm.encode: invalid instruction"

let assemble = fun prog -> Array.map encode prog

let decode2 = fun n ->
  let arg1 = n lsr 3 in
  let arg2 = n - (arg1 lsl 3) in
  arg1, arg2

let decode3 = fun n ->
  let arg1 = n lsr 6 in
  let n = n - (arg1 lsl 6) in
  let arg2 = n lsr 3 in
  let arg3 = n - (arg2 lsl 3) in
  arg1, arg2, arg3

let decode = fun n ->
  let opcode = n lsr 11 in
  let args = n - (opcode lsl 11) in
  match opcode with
  | 0x00 -> let _, r1, r2 = decode3 args in Mov (!<r1, !<r2)
  | 0x01 -> let n, r = decode2 args in IMov (Int8.decode n, !<r)
  | 0x02 -> let r1, r2, r3 = decode3 args in Add (!<r1, !<r2, !<r3)
  | 0x03 -> let r1, r2, r3 = decode3 args in Sub (!<r1, !<r2, !<r3)
  | 0x04 -> let r1, r2, r3 = decode3 args in Mul (!<r1, !<r2, !<r3)
  | 0x05 -> let r1, r2, r3 = decode3 args in Div (!<r1, !<r2, !<r3)
  | 0x07 -> let r1, r2, r3 = decode3 args in Slt (!<r1, !<r2, !<r3)
  | 0x09 -> let r1, r2, r3 = decode3 args in Seq (!<r1, !<r2, !<r3)
  | 0x0D -> let _, _, r = decode3 args in Neg (!<r)
  | 0x10 -> let a, r = decode2 args in Lda (M.addr_of_int a, !<r)
  | 0x11 -> let a, r = decode2 args in Sta (M.addr_of_int a, !<r)
  | 0x12 -> let a, _ = decode2 args in Jmp (M.addr_of_int a)
  | 0x13 -> let a, r = decode2 args in Brz (M.addr_of_int a, !<r)
  | 0x14 -> let _, _, r = decode3 args in Push (!<r)
  | 0x15 -> let _, _, r = decode3 args in Pop (!<r)
  | 0x16 -> let a, _ = decode2 args in Call (M.addr_of_int a)
  | 0x17 -> Ret
  | 0x1C -> let _, r = decode2 args in In (!<r)
  | 0x1D -> let _, r = decode2 args in Out (!<r)
  | 0x1F -> Hlt
  | _ -> Invalid

let fprint_elf = fun ch program ->
  Array.iter (fun i -> Format.fprintf ch "%04X " i) program

type state = {
    overflow : bool;
    input : string;
    input_ready : bool;
    output : int list;
    output_ready : bool;
    regs : R.file;
    mem : M.t;
    current_instr : int;
    finished : bool;
    illegal : bool;
    prog : instruction array }

let rget = fun r st -> R.get st.regs r
let rset = fun r e st -> { st with regs = R.set st.regs r e }
let incr = fun r n st -> rset r (rget r st + n) st
let decr = fun r n st -> incr r ~-n st

let store = fun a v s st -> { st with mem = M.store st.mem a v s }
let load = fun a s st -> M.fetch st.mem a s

let init_state = fun prog ->
  let mem = M.ram () in
  { overflow = false;
    input = ""; input_ready = false;
    output = []; output_ready = false;
    regs = R.file (); mem;
    current_instr = -1;
    finished = false; illegal = false;
    prog }
  |> rset SP (mem.size_bytes - 1)
  (* st.%(SP) <- st.mem.size_bytes - 1 *)

let reg_active = fun reg st ->
  0 <= st.current_instr && st.current_instr < Array.length st.prog &&
    match st.prog.(st.current_instr) with
    | IMov (_, r) | Neg r | Lda (_, r) | Sta (_, r) -> r = reg
    | Mov (r1, r2) -> r1 = reg || r2 = reg
    | Add (r1, r2, r3) | Sub (r1, r2, r3) | Mul (r1, r2, r3) | Div (r1, r2, r3)
      | Slt (r1, r2, r3) | Seq (r1, r2, r3) ->
       r1 = reg || r2 = reg || r3 = reg
    | Push r | Pop r -> r = reg || R.SP = reg
    | In r -> r = reg || R.IN = reg
    | Out r -> r = reg || R.OUT = reg
    | Jmp _ | Brz _ | Call _ | Ret -> R.PC = reg
    | Hlt | Lbl _ | LblJmp _ | LblBrz _ | Invalid -> false

let mem_active = fun addr st ->
  0 <= st.current_instr && st.current_instr < Array.length st.prog &&
    match st.prog.(st.current_instr) with
    | Mov _ | IMov _ | Neg _
      | Add _ | Sub _ | Mul _ | Div _
      | Slt _ | Seq _
      | In _ | Out _
      | Jmp _ | Brz _ | Ret | Hlt
      | Lbl _ | LblJmp _ | LblBrz _ | Invalid -> false
    | Push r ->
       let v = rget SP st in v - (R.size_bytes r - 1) <= addr && addr <= v
    | Pop r ->
       let v = rget SP st in v + 1 <= addr && addr <= v + R.size_bytes r
    | Lda (a, r) | Sta (a, r) ->
       let a = M.addr_to_int a in
       a <= addr && addr < a + R.size_bytes r
    | Call a -> addr = M.addr_to_int a

(* DRAWING *)
module Draw = struct

  (* to benefit from rainbow mode *)
  let color = fun str ->
    let value = fun s -> int_of_string ("0x" ^ s) in
    let r = value (String.sub str 1 2)
    and g = value (String.sub str 3 2)
    and b = value (String.sub str 5 2) in
    Graphics.rgb r g b

  let light_gray    = color "#C8C8C8"
  let dark_gray     = color "#2A2A2A"
  let program_color = color "#C6E2FE"
  let data_color    = color "#A9E8C2"
  let heap_color    = color "#CEA9E8"
  let stack_color   = color "#FFBFBA"
  let hilite_color  = color "#FF0000"

  type align = Left | Center | Right

  let cell = fun x y sx sy ?fg ?bg ?border ?(align = Center) text ->
    begin match bg with
    | None -> ()
    | Some color -> Graphics.set_color color; Graphics.fill_rect x y sx sy end;
    begin match border with
    | None -> ()
    | Some color ->
       Graphics.set_color color; Graphics.set_line_width 2;
       Graphics.draw_rect x (y + 1) (sx - 1) (sy - 2);
       Graphics.set_line_width 1
    end;
    begin match fg with
    | None -> Graphics.set_color Graphics.foreground
    | Some color -> Graphics.set_color color end;
    let tw, th = Graphics.text_size text in
    let margin = 5 in
    let xtext =
      match align with
      | Left -> x + margin
      | Center -> x + (sx - tw) / 2
      | Right -> x + sx - tw - margin in
    Graphics.moveto xtext (y + (sy - th) / 2);
    Graphics.draw_string text;
    Graphics.set_color Graphics.foreground

  let reg = fun st x y sx sy ri v ->
    let r = !<ri in
    let sr = R.size r in
    let sx_data = 100 in
    let alpha, beta = if sr < 4 then 140, 60 else 100, 100 in
    let sx, sx' = alpha * sx / 100, beta * sx / 100 in
    let fg = Graphics.white and bg = dark_gray in
    cell x y sx sy ~fg ~bg ~align:Left (R.to_string r);
    let border = if reg_active r st then hilite_color else dark_gray in
    cell (x + sx) y sx' sy ~border ~align:Right (Printf.sprintf "%0*X" sr v);
    let reg_val = match R.value_type r with
      | R.Addr -> ""
      | R.Instr -> rget IR st |> decode |> sprint_instr
      | R.Unsigned -> string_of_int v
      | R.Signed -> string_of_int (Int16.decode v) in
    cell (x + sx + sx') y sx_data sy ~align:Left reg_val

  let registers = fun x y st ->
    let sx, sy = 50, 25 in
    let x, y = x, y - sy in
    R.iteri (fun ri v -> reg st x (y - ri * sy) sx sy ri v) st.regs

  let flag = fun x y active text ->
    let sbox = 10 and slight = 6 in
    let offset = (sbox - slight) / 2 in
    let margin = 5 in
    Graphics.draw_rect x y sbox sbox;
    if active then begin
        Graphics.set_color hilite_color;
        Graphics.fill_rect (x + offset) (y + offset) slight slight;
        Graphics.set_color Graphics.foreground end;
    Graphics.moveto (x + sbox + margin) (y - 2);
    Graphics.draw_string text

  let flags = fun x y st ->
    let sep = 20 in
    flag x y st.overflow "Overflow";
    flag x (y - sep) st.input_ready "Input ready";
    flag x (y - 2 * sep) st.illegal "Illegal instruction";
    flag x (y - 3 * sep) st.finished "Halt"

  let memory_cell_sizex = 5 + 20 * M.block_size_bytes
  let memory_cell_sizey = 20
  let memory = fun x y st ->
    let sx, sy = memory_cell_sizex, memory_cell_sizey in
    let x, y = x, y - sy in
    for i = 0 to 15 do
      let bg = dark_gray and fg = Graphics.white in
      cell x (y - (i + 1) * sy) sx sy ~fg ~bg (Printf.sprintf "%X_" i);
      cell (x + (i + 1) * sx) y sx sy ~fg ~bg (Printf.sprintf "_%X" i)
    done;
    for addr = 0 to st.mem.size_bytes do
      let xi = x + (1 + addr mod 16) * sx in
      let yi = y - (1 + addr / 16) * sy in
      let data = M.get st.mem (M.addr_of_int addr) in
      let bg =
        if addr < M.addr_to_int st.mem.data_start then program_color
        else if addr < M.addr_to_int st.mem.heap_start then data_color
        else if addr < M.addr_to_int st.mem.free_start then heap_color
        else if addr > rget SP st then stack_color
        else Graphics.background in
      let sdata = M.sprint_data data in
      if mem_active addr st then
        cell xi yi sx sy ~bg ~border:hilite_color sdata
      else cell xi yi sx sy ~bg sdata
    done

  let memory_legend = fun x y ->
    let sx, sy = 80, memory_cell_sizey in
    cell x (y - sy) sx sy ~bg:dark_gray ~fg:Graphics.white ~align:Left "Address";
    cell x (y - 2 * sy) sx sy ~bg:program_color ~align:Left "Program";
    cell x (y - 3 * sy) sx sy ~bg:data_color ~align:Left "Data";
    cell x (y - 4 * sy) sx sy ~bg:heap_color ~align:Left "Heap";
    cell x (y - 5 * sy) sx sy ~bg:stack_color ~align:Left "Stack"

  let screen_sizex = 300
  let screen_sizey = 3 * screen_sizex / 4
  let screen = fun x y st ->
    let sx, sy = screen_sizex, screen_sizey in
    let sep = 18 and margin = 5 in
    Graphics.set_color dark_gray;
    Graphics.fill_rect x y sx sy;
    Graphics.set_color Graphics.green;
    Graphics.moveto (x + margin) (y + sy - sep);
    List.iter (fun d ->
        let sd = Printf.sprintf "> %d" d in
        let w, _ = Graphics.text_size sd in
        Graphics.draw_string sd;
        Graphics.rmoveto ~-w ~-sep
      ) st.output;
    Graphics.draw_string "> ";
    Graphics.set_color Graphics.foreground

  let input_sizex = screen_sizex
  let input_sizey = 25
  let input = fun x y st ->
    let sx, sy = input_sizex, input_sizey in
    Graphics.set_line_width 2;
    cell x y sx sy ~border:dark_gray ~align:Right st.input;
    Graphics.set_line_width 1

  let program = fun x y st ->
    let sy = 18 (*20*) in
    let x, y = x, y - sy in
    Array.iteri
      (fun ni instr ->
        if ni = st.current_instr then begin
            Graphics.set_color Graphics.foreground;
            Graphics.set_font "9x15bold" end
        else if ni = rget PC st / nb_instr_chunks then begin
            Graphics.set_color Graphics.foreground;
            Graphics.set_font "9x15" end
        else begin Graphics.set_color light_gray; Graphics.set_font "9x15" end;
        let text = sprint_instr (* ~k:ni *) instr in
        Graphics.moveto x (y - sy * ni);
        Graphics.draw_string text
      ) st.prog;
    Graphics.set_color Graphics.foreground

  let section_title = fun x y title ->
    Graphics.set_font "9x15bold";
    Graphics.moveto x y;
    Graphics.draw_string title;
    Graphics.set_font "9x15"

  let simu = fun st ->
    Graphics.clear_graph ();
    Graphics.set_font "9x15";
    let xmax = Graphics.size_x () in
    let ymax = Graphics.size_y () and y_offset = 20 in
    let mem_sizex = 17 * memory_cell_sizex
    and mem_sizey = 17 * memory_cell_sizey in
    let code_x = xmax - 200 in
    let io_x, io_y = code_x - screen_sizex, ymax - screen_sizey in
    memory 0 mem_sizey st;
    memory_legend (mem_sizex + 20) mem_sizey;
    registers 20 (ymax - y_offset) st;
    flags 20 (ymax - 320) st;
    screen io_x io_y st;
    input io_x (io_y - input_sizey) st;
    section_title (code_x + 20) (ymax - y_offset) "Code";
    program (code_x + 20) (ymax - y_offset) st;
    Graphics.set_color light_gray;
    Graphics.moveto code_x 0;
    Graphics.lineto code_x ymax;
    Graphics.set_color Graphics.foreground

  let refresh = fun st -> simu st; Graphics.synchronize ()

  let wait = fun () ->
    match Graphics.read_key () with
    | '\027'| 'q' -> raise Exit
    | _ -> ()

end

let rec input = fun st ->
  Draw.refresh st;
  match Graphics.read_key () with
  | '\027'| 'q' -> raise Exit
  | '-' -> if st.input = "" then input { st with input  = "-" } else input st
  | '0'..'9' as c -> input { st with input = st.input ^ Char.escaped c }
  | '\b' ->
     if st.input = "" then input st
     else
       let n = String.length st.input in
       input { st with input = String.sub st.input 0 (n - 1) }
  | '\r' | '\n' ->
     begin
       try
         let v = int_of_string st.input in
         let st = rset IN (Int16.encode v) st in
         { st with input = ""; input_ready = true }
       with _ -> Printf.eprintf "Invalid input\n%!"; { st with input = "" }
     end
  | _ -> input st

let comp = fun op -> fun x y -> if op x y then 1 else 0

let exec_binop = fun st r1 r2 r3 op ->
  let raw = op (rget r2 st) (rget r3 st) in
  let v = Int16.encode raw in
  let st = rset r1 v st in
  if Int16.decode v = raw then st else { st with overflow = true }

let exec_instr = fun instr st ->
  match instr with
  | Mov (r1, r2) -> rset r1 (rget r2 st) st
  | IMov (n, r) -> rset r n st
  | Add (r1, r2, r3) -> exec_binop st r1 r2 r3 ( + )
  | Sub (r1, r2, r3) -> exec_binop st r1 r2 r3 ( - )
  | Mul (r1, r2, r3) -> exec_binop st r1 r2 r3 ( * )
  | Div (r1, r2, r3) -> exec_binop st r1 r2 r3 ( / )
  | Slt (r1, r2, r3) -> exec_binop st r1 r2 r3 (comp (<))
  | Seq (r1, r2, r3) -> exec_binop st r1 r2 r3 (comp (=))
  | Neg r -> rset r (if rget r st = 0 then 1 else 0) st
  | Lda (a, r) -> rset r (load a (R.size_bytes r) st) st
  | Sta (a, r) -> store a (rget r st) (R.size_bytes r) st
  | Jmp a -> rset PC (M.addr_to_int a) st
  | Brz (a, r) -> if rget r st = 0 then rset PC (M.addr_to_int a) st else st
  | Push r ->
     let s = R.size_bytes r in
     store (M.addr_of_int (rget SP st + 1 - s)) (rget r st) s st |> decr SP s
  | Pop r ->
     let s = R.size_bytes r in
     incr SP s st
     |> rset r (load (M.addr_of_int (rget SP st + 1 - s)) s st)
  | In r ->
     if st.input_ready
     then { st with input_ready = false } |> rset r (rget IN st)
     else st |> decr CC 1 |> decr PC 1
  | Out r -> { st with output_ready = true} |> rset OUT (rget r st)
  | Hlt -> { st with finished = true }
  | Lbl _ | LblJmp _ | LblBrz _ -> st
  | Invalid -> { st with illegal = true }
  | _ -> st (* FIXME *)

let exec_output = fun st ->
  if not st.output_ready then st
  else { st with output_ready = false; output = st.output @ [rget OUT st] }

let fetch = fun st ->
  { st with current_instr = rget PC st / nb_instr_chunks }
  |> rset IR (load (M.addr_of_int (rget PC st)) instr_size_bytes st)
  |> incr PC nb_instr_chunks

let exec_cycle = fun ?(draw = true) st ->
  let st = st |> incr CC 1 |> fetch |> exec_output in
  if draw then begin Draw.refresh st; Draw.wait () end;
  exec_instr (rget IR st |> decode) st

let rec exec = fun history st ->
  Draw.refresh st;
  match Graphics.read_key () with
  | '\027'| 'q' -> raise Exit
  (* | 'l' ->
   *    let p = Array.map encode st.prog in
   *    exec history
   *      { st with
   *        illegal = false;
   *        mem = M.load_program st.mem p instr_size_bytes } *)
  | ' ' ->
     if st.finished || st.illegal then exec history st
     else exec (st :: history) (exec_cycle st)
  | '\b' ->
     begin match history with
     | [] -> exec history st
     | old_st :: old_history -> exec old_history old_st end
  | '\r' | '\n' -> exec history (input st)
  | _ ->  exec history st

let rec quick_exec = fun st ->
  if st.finished then Printf.printf "\nResult: %d\n%!" (rget AX st)
  else if st.illegal then Printf.eprintf "Wrong OP code\n%!"
  else quick_exec (exec_cycle ~draw:false st)

let exec = fun ?(fast = false) program ->
  Printf.printf "\nProduced assembly code:\n";
  fprint_asm Format.std_formatter program;
  let prog = make_program program in
  Printf.printf "%!\nAfter branching:\n";
  Array.iter (fun i -> Printf.printf "%s\n" (sprint_instr i)) prog;
  Printf.printf "%!\nProduced machine code:\n";
  fprint_elf Format.std_formatter (assemble prog);
  Printf.printf "\n%!";
  let init_state = init_state prog in
  let p = Array.map encode init_state.prog in
  let st = { init_state with
             illegal = false;
             mem = M.load_program init_state.mem p instr_size_bytes } in
  if fast then quick_exec st
  else
    try
      Graphics.open_graph " 1000x800";
      Graphics.set_window_title "ASM processor simulation";
      Graphics.auto_synchronize false;
      Draw.refresh st;
      exec [] st
    with Exit | Graphics.Graphic_failure _ -> ()

(** TODO: add mechanism to have slower access on memory than on registers *)
(** TODO : ?? *)
