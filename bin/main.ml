open Cil
open Pretty
open Lib.Utils
open Lib.Types

module E = Errormsg
module TS = Lib.Typesig

let display_alt_types types =
  let type_list =
    let pretty_print cur type_list =
      cur ^ "[" ^ (string_of_string_list type_list) ^ "] " in
    List.fold_left pretty_print "" types in
  E.log "Alternate types: %s\n" type_list

let function_info glob =
  match glob with
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
     E.log "Formal types: [%s]\n" (list_to_string pp_variable_type f.sformals);
     E.log "Formal sig: [%s]\n" (TS.string_of_sig formals_sig);
     display_alt_types formal_alts;
     E.log "Local types: [%s]\n" (list_to_string pp_variable_type f.slocals);
     E.log "Local sig: [%s]\n" (TS.string_of_sig locals_sig);
     display_alt_types local_alts
  | _ -> ()

let add_base_types () =
  let add t = TS.add_type (TS.offsets_of_type t) (string_of_type t) in
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

let main () =
  initCIL ();
  let fname = Sys.argv.(1) in
  let parsed = parse_one_file fname in
  add_base_types ();
  iterGlobals parsed collect_types;
  let print_fun_info f =
    match f with
    | GFun _ -> function_info f; E.log "\n"
    | _ -> () in
  iterGlobals parsed print_fun_info;
  TS.print_types ()
;;

main ();
exit 0
