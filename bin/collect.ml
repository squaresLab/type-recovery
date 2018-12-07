open Cil
open Utils

module TS = Typesig

let main() =
  initCIL ();
  TS.add_base_types TS.global_signatures;
  let fnames =
    match Array.to_list Sys.argv with
    | [ _ ] | [] -> failwith "no input files"
    | _ :: files -> files
  in
  Printf.printf "%d input files:\n" (List.length fnames);
  let process_file fname =
    Printf.printf "Processing %s\n%!" fname;
    let parsed = parse_one_file fname in
    TS.collect_types parsed;
  in
  List.iter process_file fnames;
  TS.print_types TS.global_signatures;
;;

main ();
exit 0
