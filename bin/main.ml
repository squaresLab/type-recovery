open Cil
open Pretty
open Utils
open Sexplib
open Sexplib.Std

module E = Errormsg
module TS = Typesig

let processed_files : was_seen = Hashtbl.create 3
let trained_files = Hashtbl.create 3

let display_alt_types types =
  let type_list =
    let pretty_print cur type_list =
      cur ^ "[" ^ (string_of_string_list type_list) ^ "] " in
    List.fold_left pretty_print "" types in
  E.log "Alternate types: %s\n" type_list

let read_io_pairs fname =
  let in_channel = open_in fname in
  let sexp = Sexp.input_sexp in_channel in
  let converted = list_of_sexp (list_of_sexp string_of_sexp) sexp in
  let pair_of_list l =
    match l with
    | l1 :: l2 :: [] -> (l1, l2)
    | _ -> failwith (Printf.sprintf "Error reading %s: invalid format" fname)
  in
  List.map pair_of_list converted

let save_io_pairs (pairs : (string * string) list) fname =
  let list_of_pair (p1, p2) = [p1; p2] in
  let out_channel = open_out fname in
  let converted = List.map list_of_pair pairs in
  let sexp = sexp_of_list (sexp_of_list sexp_of_string) converted in
  Sexp.output_mach out_channel sexp;
  close_out out_channel

let process_file filename =
  Printf.printf "Processing %s\n%!" filename;
  let pairs_file = "io-pairs/" ^ (Filename.basename filename) in
  if Sys.file_exists pairs_file then
    Printf.printf "Found pairs file, skipping\n%!"
  else begin
      let parsed = parse_one_file filename in
      TS.collect_types parsed;
      let fhash = Fileinfo.get_file_hash filename in
      let typemap = Hashtbl.find TS.file_signatures fhash in
      let signature = [TS.Data 32] in
      let types = Hashtbl.find typemap signature in
      let io_pairs = Lex.tokenize_training_pairs types filename in
      save_io_pairs io_pairs pairs_file
    end

let save_info () =
  TS.to_file "typesig.txt";
  Lex.save_vocabs "vocab.txt"

let load_info () =
  let load f file =
    try
      Printf.printf "Loading %s... " file;
      f file;
      Printf.printf "OK\n"
    with _ ->
      Printf.printf "Failed\n"
  in
  load TS.from_file "typesig.txt";
  load Lex.load_vocabs "vocab.txt";
  load (Fileinfo.from_file trained_files) "trained.txt"


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
  let time = Unix.time () in
  let {Unix.tm_sec=seconds; tm_min=minutes; tm_hour=hours;
       tm_mday=day_of_month; tm_mon=month; tm_year=year;
       tm_wday=wday; tm_yday=yday; tm_isdst=isdst} =
    Unix.localtime time
  in
  let now = Printf.sprintf "%02d:%02d:%02d - %02d/%02d/%04d"
              hours minutes seconds (month + 1) day_of_month (year + 1900)
  in
  let out_channel = open_out "stats.txt" in
  Printf.fprintf out_channel "New run started %s\n" now;
  Printf.fprintf out_channel "%d input files:\n" (List.length fnames);
  List.iter (Printf.fprintf out_channel "%s\n") fnames;
  Printf.fprintf out_channel "----------\n";
  close_out_noerr out_channel;
  List.iter (fun name -> process_file name; save_info ()) fnames;
  let input_vocab =
    Hashtbl.fold (fun k _ vocab -> k :: vocab) Lex.input_vocab []
  in
  let output_vocab =
    Hashtbl.fold (fun k _ vocab -> k :: vocab) Lex.output_vocab ["<--->"]
  in
  let train_one_file fnum fname =
    let basename = Filename.basename fname in
    let pairs_file = "io-pairs/" ^ basename in
    let pairs_file_hash = Fileinfo.get_file_hash pairs_file in
    let num_files = List.length fnames in
    let file_info_string =
      Printf.sprintf "File %d of %d: %s" (fnum + 1) num_files basename
    in
    if Hashtbl.mem trained_files pairs_file_hash then
      let info =
        Printf.sprintf "%s\nAlready trained, skipping\n" file_info_string
      in
      let out_channel = open_out_gen [Open_append] 0o664 "stats.txt" in
      Printf.fprintf out_channel "%s" info;
      close_out_noerr out_channel;
      Printf.printf "%s" info
    else begin
        let io_pairs = [read_io_pairs pairs_file] in
        NN.init file_info_string input_vocab output_vocab io_pairs ();
        NN.run_dynet ();
        Hashtbl.replace trained_files pairs_file_hash true;
        Fileinfo.to_file trained_files "trained.txt"
      end
  in
  List.iteri train_one_file fnames
;;

main ();
exit 0
