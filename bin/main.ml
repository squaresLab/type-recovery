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

let process_file filename =
  Printf.printf "Processing %s\n%!" filename;
  let parsed = parse_one_file filename in
  TS.collect_types parsed;
  let fhash = Lib.Fileinfo.get_file_hash filename in
  let typemap = Hashtbl.find TS.file_signatures fhash in
  let signature = [TS.Data 32] in
  let types = Hashtbl.find typemap signature in
  Lex.tokenize_training_pairs types filename

let save_info () =
  TS.to_file "typesig.txt"

let load_info () =
  try
    TS.from_file "typesig.txt"
  with _ ->
    ()

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

let rec menu () =
  let opts = ["exit"; "something else"] in
  Printf.printf "menu\n";
  List.iteri (fun i -> Printf.printf "%d: %s\n" (i+1)) opts;
  let rec get_selection () =
    let input = read_line () in
    try
      int_of_string input
    with Failure _ -> begin
          Printf.printf "%s is not a number, try again.\n" input;
          get_selection ()
      end
  in
  let selection = get_selection () in
  match selection with
  | 1 -> ()
  | 2 -> Printf.printf "Cool!\n"; menu ()
  | _ -> Printf.printf "invalid selection %d\n" selection; menu ()

let main () =
  initCIL ();
  TS.add_base_types TS.global_signatures;
  load_info ();
  let fnames =
    match Array.to_list Sys.argv with
    | [ _ ]  | [] -> failwith "Error: no input files"
    | _ :: files -> files
  in
  let collect_pairs collected fname =
    let new_pairs = process_file fname in
    new_pairs :: collected
  in
  let io_pairs = List.fold_left collect_pairs [] fnames in
  save_info ();
  NN.init io_pairs ();
  NN.test_dynet()
;;

main ();
exit 0
