open Cil
open Utils
open Sexplib
open Sexplib.Std
open Types

module E = Errormsg

let version_number = 1

type memory = | Data of int
              | Padding of int [@@deriving sexp]

type tsig = memory list [@@deriving sexp]

type sigmap = (tsig, string list) Hashtbl.t [@@deriving sexp]
type filemap = (string, sigmap) Hashtbl.t [@@deriving sexp]

let global_signatures : sigmap = Hashtbl.create 3
let file_signatures : filemap = Hashtbl.create 3

let seen_files = Fileinfo.empty

let header = Printf.sprintf "typesig file v%d" version_number

let add_to_signatures type_sig name signature_table =
  let cur_types = Hashtbl.find_opt signature_table type_sig in
  match cur_types with
  | None -> begin
      Hashtbl.replace signature_table type_sig [name]
    end
  | Some ts when not (List.mem name ts) ->
     Hashtbl.replace signature_table type_sig (name :: ts)
  | _ -> ()

let string_of_sig (t : tsig) =
  let string_of_memory = function
    | Data x -> string_of_int x
    | Padding x -> Printf.sprintf "P%d" x in
  string_of_list string_of_memory t

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

let add_base_types () =
  let add t =
    let signature = offsets_of_type t in
    let name = string_of_type t in
    add_to_signatures signature name global_signatures
  in
  let base_pointer_types = add_pointers_to base_types in
  List.iter add (base_types @ base_pointer_types)

let get_type_names signature_table type_sig =
  match Hashtbl.find_opt signature_table type_sig with
  | None -> ["None"]
  | Some ts -> ts

let get_alt_types signature_table (type_sig : tsig) =
  let signature_partitions =
    let valid_signature =
      List.for_all (fun s -> Hashtbl.mem signature_table s) in
    List.filter valid_signature (list_partitions type_sig)
  and subtypes type_lists part =
    let part_types =
      List.map (fun s -> Hashtbl.find signature_table s) part in
    (product part_types) @ type_lists in
  List.fold_left subtypes [] signature_partitions

let print_types signature_table =
  let print_types_for_signature type_sig type_names =
    E.log "Types with signature [%s]: %s\n"
      (string_of_sig type_sig)
      (string_of_string_list type_names) in
  Hashtbl.iter print_types_for_signature signature_table

let collect_types cilfile =
  let fhash = Fileinfo.get_file_hash cilfile.fileName in
  if Fileinfo.saw_file seen_files fhash then
    ()
  else
    begin
      let file_signatures =
        match Hashtbl.find_opt file_signatures fhash with
        | Some signatures -> signatures
        | None ->
           let signatures = Hashtbl.create 3 in
           Hashtbl.replace file_signatures fhash signatures;
           signatures
      in
      let add_type type_sig name =
        add_to_signatures type_sig name global_signatures;
        add_to_signatures type_sig name file_signatures
      in
      let collect_one_type = function
        | GType (t, _) ->
           let type_sig = offsets_of_type t.ttype in
           add_type type_sig t.tname
        | GCompTag (cinfo, _) ->
           let ttype = TComp (cinfo, []) in
           let type_sig = offsets_of_type ttype in
           add_type type_sig cinfo.cname
        (* Enums probably need to be treated differently *)
        | GEnumTag (einfo, _) ->
           let ttype = TEnum (einfo, []) in
           let type_sig = offsets_of_type ttype in
           add_type type_sig einfo.ename
        | _ -> ()
      in
      iterGlobals cilfile collect_one_type;
      Fileinfo.add_file seen_files fhash;
    end

let to_file fname =
  let processed = Sexp.to_string (Fileinfo.to_sexp seen_files) in
  let global = Sexp.to_string (sexp_of_sigmap global_signatures) in
  let file = Sexp.to_string (sexp_of_filemap file_signatures) in
  let out_channel = open_out fname in
  Printf.fprintf out_channel "%s\n%s\n%s\n%s\n" header processed global file;
  close_out out_channel

let from_file fname =
  let in_channel = open_in fname in
  let h = input_line in_channel in
  if h <> header then
    raise (Failure (Printf.sprintf "invalid file format %s" fname));
  let processed_string = input_line in_channel in
  let global_string = input_line in_channel in
  let file_string = input_line in_channel in
  Fileinfo.from_sexp seen_files (Sexp.of_string processed_string);
  let gs = sigmap_of_sexp (Sexp.of_string global_string) in
  Hashtbl.iter (fun key v -> Hashtbl.replace global_signatures key v) gs;
  let fs = filemap_of_sexp (Sexp.of_string file_string) in
  Hashtbl.iter (fun key v -> Hashtbl.replace file_signatures key v) fs
