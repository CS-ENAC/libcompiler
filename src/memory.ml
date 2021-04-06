type address = int

let address_size_bytes = 1
let address_size_nybbles = 2 * address_size_bytes
let address_size = 1 lsl (8 * address_size_bytes)

let addr_to_int = fun addr -> addr
let addr_of_int = fun n -> n mod address_size

let sprint_addr = fun a -> Printf.sprintf "[%0*X]" address_size_nybbles a
let fprint_addr = fun ch a -> Format.fprintf ch "%s" (sprint_addr a)

(* Number of blocks *)
let size_bytes = 1 lsl (8 * address_size_bytes)

let block_size_bytes = 1
let block_size_nybbles = 2 * block_size_bytes
let block_size = 1 lsl (8 * block_size_bytes)

let random_data = fun () -> Random.int block_size
let sprint_data = fun x -> Printf.sprintf "%0*X" block_size_nybbles x

module Operators = struct
  let ( +@ ) = ( + )
end

exception Unbound of string

let v2a : (string, address) Hashtbl.t = Hashtbl.create 17
let a2v : (address, string) Hashtbl.t = Hashtbl.create 17
let new_address =
  let a = ref 0 in
  fun v ->
  let addr = !a in
  a := !a + 2 * block_size_bytes;
  Hashtbl.add v2a v addr;
  Hashtbl.add a2v addr v; addr

let get_address = fun v ->
  try Hashtbl.find v2a v with Not_found -> raise (Unbound v)

(* !FIXME! *)
let static_data_size = fun () -> 2 * Hashtbl.length v2a

type ram = int array
type t = { memory : ram;
           size_bytes : int;
           code_start : address;
           data_start : address;
           heap_start : address;
           free_start : address }

let ram = fun () ->
  { memory = Array.init size_bytes (fun _ -> random_data ());
    size_bytes; code_start = 0; data_start = 0; heap_start = 0; free_start = 0 }

let get = fun m addr -> m.memory.(addr)

let copy_ram = fun m -> { m with memory = Array.copy m.memory }

let fetch = fun m addr size_bytes ->
  let nb_chunks = size_bytes / block_size_bytes in
  let data = ref 0 in
  for k = 0 to nb_chunks - 1 do
    data := !data + (m.memory.(addr + nb_chunks - 1 - k) lsl (8 * k))
  done; !data

let store_aux = fun ram addr data data_bytes ->
  let nb_chunks = data_bytes / block_size_bytes in
  let data_rest = ref data in
  for k = nb_chunks - 1 downto 0 do
    let shift = 8 * k * block_size_bytes in
    let chunk_k = !data_rest lsr shift in
    ram.(addr + nb_chunks - 1 - k) <- chunk_k;
    data_rest := !data_rest - chunk_k lsl shift
  done

let store = fun m addr data data_bytes ->
  let m = copy_ram m in store_aux m.memory addr data data_bytes; m

let load_program = fun m program instr_size_bytes ->
  let prog_size = Array.length program * instr_size_bytes in
  let data_size = static_data_size () in
  let nb_chunks = instr_size_bytes / block_size_bytes in
  let m = copy_ram { m with data_start = prog_size;
                            heap_start = prog_size + data_size;
                            free_start = prog_size + data_size } in
  Array.iteri
    (fun i instr -> store_aux m.memory (nb_chunks * i) instr instr_size_bytes
    ) program; m
