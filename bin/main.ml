open Cil
open Pretty
open Lib.Utils
open Lib.Types

module E = Errormsg
module Lex = Lib.Lex
module NN = Lib.Neuralnet
module TS = Lib.Typesig

let processed_files : was_seen = Hashtbl.create 3

let display_alt_types types =
  let type_list =
    let pretty_print cur type_list =
      cur ^ "[" ^ (string_of_string_list type_list) ^ "] " in
    List.fold_left pretty_print "" types in
  E.log "Alternate types: %s\n" type_list

let function_info = function
  | GFun (f, loc) ->
     let return_type =
       match f.svar.vtype with
       | TFun (t, _, _, _) -> t
       | _ -> voidType
     in
     let alt_types = TS.get_alt_types (TS.offsets_of_type return_type) in
     E.log "Function: %s has return type %a\n" f.svar.vname d_type return_type;
     display_alt_types alt_types;

     let collect_formals sigs formal = sigs @ TS.offsets_of_type formal.vtype in
     let formals_sig = List.fold_left collect_formals [] f.sformals in

     let collect_locals sigs local = sigs @ (TS.offsets_of_type local.vtype) in
     let locals_sig = List.fold_left collect_locals [] f.slocals in

     let formal_alts = TS.get_alt_types formals_sig in
     let local_alts = TS.get_alt_types locals_sig in

     let pp_variable_type v = sprint 10 (dprintf "%a" d_type v.vtype) in
     E.log "Formal types: [%s]\n" (string_of_list pp_variable_type f.sformals);
     E.log "Formal sig: [%s]\n" (TS.string_of_sig formals_sig);
     display_alt_types formal_alts;
     E.log "Local types: [%s]\n" (string_of_list pp_variable_type f.slocals);
     E.log "Local sig: [%s]\n" (TS.string_of_sig locals_sig);
     display_alt_types local_alts
  | _ -> ()

let add_base_types () =
  let add t = TS.add_type (TS.offsets_of_type t) (string_of_type t) in
  let base_pointer_types = add_pointers_to base_types in
  List.iter add (base_types @ base_pointer_types)

let collect_types = function
  | GType (t, _) ->
     let type_sig = TS.offsets_of_type t.ttype in
     TS.add_type type_sig t.tname
  | GCompTag (cinfo, _) ->
     let ttype = TComp (cinfo, []) in
     let type_sig = TS.offsets_of_type ttype in
     TS.add_type type_sig cinfo.cname
  (* Enums probably need to be treated differently *)
  | GEnumTag (einfo, _) ->
     let ttype = TEnum (einfo, []) in
     let type_sig = TS.offsets_of_type ttype in
     TS.add_type type_sig einfo.ename
   | _ -> ()

let process_file tokenized_files filename =
  Printf.printf "Processing %s\n%!" filename;
  let tokens = Lex.tokenize filename in
  (* List.iter (fun token -> Hashtbl.replace vocab_tbl token true) tokens; *)
  (* let parsed = parse_one_file filename in
   * iterGlobals parsed collect_types; *)
  (* let file_hash = Digest.file filename in
   * Hashtbl.add processed_files file_hash true; *)
  tokens :: tokenized_files

let save_info () =
  Lex.save_vocab "vocab.txt"

let load_info () =
  Lex.load_vocab "vocab.txt"

(* FIXME *)
let print_help () =
  let program_name = Sys.argv.(0) in
  Printf.printf "Usage: %s [files]\n" program_name;
  exit 0

(* FIXME *)
let parse_args () =
  let output_dir s = () in
  let speclist = [("-o", Arg.String output_dir, "Output dir")] in
  let usage_message = "Train a neural net" in
  Arg.parse speclist print_endline usage_message;
  exit 0

let main () =
  load_info ()
  (* initCIL ();
   * add_base_types ();
   * let fnames =
   *   match Array.to_list Sys.argv with
   *   | [ _ ]  | [] -> failwith "Error: no input files"
   *   | _ :: files -> files
   * in
   * let tokenized_files = List.fold_left process_file [] fnames in *)
  (* Printf.printf "Vocab size: %d\n" (Hashtbl.stats vocab_tbl).num_bindings;
   * let vocab =
   *   Hashtbl.fold (fun token _ tokens -> token :: tokens) vocab_tbl [] in
   * let vocab = "<???>" :: vocab in
   * let replace_tokens signature token_names tokenized_file =
   *   let token_placeholder = "<???>" in
   *   let replace_token token =
   *     if List.mem token token_names then (token_placeholder, token)
   *     else (token, token_placeholder)
   *   in
   *   List.map replace_token tokenized_file
   * in
   * let target_sig = [TS.Data 32] in
   * let types = Hashtbl.find TS.signatures target_sig in
   * let io_pairs = List.map (replace_tokens target_sig types) tokenized_files in *)
  (* NN.init vocab types io_pairs ();
   * NN.test_dynet() *)
;;

main ();
exit 0
