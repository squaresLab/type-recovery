open Cil
open Utils
open Sexplib
open Sexplib.Std

module E = Errormsg

type memory = | Data of int
              | Padding of int [@@deriving sexp]

type tsig = memory list [@@deriving sexp]

type sigmap = (tsig, string list) Hashtbl.t [@@deriving sexp]

let signatures : sigmap = Hashtbl.create 3

let string_of_sig (t : tsig) =
  let string_of_memory = function
    | Data x -> string_of_int x
    | Padding x -> Printf.sprintf "P%d" x in
  list_to_string string_of_memory t

let offsets_of_type (t : typ) : tsig =
  match t with
  | TArray (base_type, exp, attrs) ->
     let base_type_size = bitsSizeOf base_type in
     begin
       match exp with
       | None -> []
       | Some e ->
          begin
            match isInteger e with
            | None -> failwith "Expression is not an integer\n"
            | Some i -> repeat (i64_to_int i) (Data base_type_size)
          end
     end
  | TComp (cinfo, _) when cinfo.cstruct ->
     let align_bits = 8 * (alignOf_int t) in
     let add_padded_memory (offsets, cur_padding) field =
       let tsize = bitsSizeOf field.ftype in
       let next_padding =
         match tsize mod align_bits with
         | 0 -> 0
         | t -> align_bits - t in
       match cur_padding, field.ftype with
       | 0, _ -> ((Data tsize) :: offsets, next_padding)
       | p, TInt (IChar, _) -> ((Data 8) :: offsets, p - 8)
       | p, t -> ((Data tsize) :: (Padding p) :: offsets, next_padding) in
     let (info, _) = List.fold_left add_padded_memory ([], 0) cinfo.cfields in
     List.rev info
  | _ -> [Data (bitsSizeOf t)]

let add_type type_sig name =
  let cur_types = Hashtbl.find_opt signatures type_sig in
  match cur_types with
  | None -> Hashtbl.replace signatures type_sig [name]
  | Some ts when not (List.mem name ts) ->
     Hashtbl.replace signatures type_sig (name :: ts)
  | _ -> ()

let get_type_names type_sig =
  match Hashtbl.find_opt signatures type_sig with
  | None -> ["None"]
  | Some ts -> ts

let get_alt_types (type_sig : tsig) =
  let signature_partitions =
    let valid_signature = List.for_all (fun s -> Hashtbl.mem signatures s) in
    List.filter valid_signature (list_partitions type_sig)
  and subtypes type_lists part =
    let part_types = List.map (fun s -> Hashtbl.find signatures s) part in
    (product part_types) @ type_lists in
  List.fold_left subtypes [] signature_partitions

let print_types () =
  let print_types_for_signature type_sig type_names =
    E.log "Types with signature [%s]: %s\n"
      (string_of_sig type_sig)
      (string_of_string_list type_names) in
  Hashtbl.iter print_types_for_signature signatures

let to_file fname = Sexp.save fname (sexp_of_sigmap signatures)

let from_file fname =
  let s = sigmap_of_sexp (Sexp.load_sexp fname) in
  Hashtbl.iter (fun key v -> Hashtbl.replace signatures key v) s
