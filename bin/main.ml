open Cil
open Pretty
open Lib.Utils
open Lib.Types

module E = Errormsg
module TS = Lib.Typesig

let display_alt_types types =
  let type_list = List.fold_left (fun cur typelist ->
                      cur ^ "[" ^ (string_of_string_list typelist) ^ "] "
                    ) "" types
  in
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
     let formals_sig = List.fold_left (fun sigs formal ->
                           let formal_sig = TS.offsets_of_type formal.vtype in
                           sigs@formal_sig
                         ) [] f.sformals
     in
     let locals_sig = List.fold_left (fun sigs local ->
                          let local_sig = TS.offsets_of_type local.vtype in
                          sigs@local_sig
                        ) [] f.slocals
     in
     let formal_alts = TS.get_alt_types formals_sig in
     let local_alts = TS.get_alt_types locals_sig in
     E.log "Formal types: [%s]\n" (list_to_string
                                     (fun f ->
                                       sprint 10 (dprintf "%a" d_type f.vtype)
                                     ) f.sformals);
     E.log "Formal sig: [%s]\n" (TS.string_of_sig formals_sig);
     display_alt_types formal_alts;
     E.log "Local types: [%s]\n" (list_to_string
                                    (fun f ->
                                      sprint 10 (dprintf "%a" d_type f.vtype)
                                    ) f.slocals);
     E.log "Local sig: [%s]\n" (TS.string_of_sig locals_sig);
     display_alt_types local_alts
  | _ -> ()

let add_base_types () =
  List.iter (fun t ->
      let type_sig = TS.offsets_of_type t in
      TS.add_type type_sig (string_of_type t)
    ) (base_types @ base_pointer_types)

let collect_types glob =
  match glob with
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
  iterGlobals parsed (fun f ->
      match f with
      | GFun _ -> function_info f; E.log "\n"
      | _ -> ()
    );
  TS.print_types ()
;;

main ();
exit 0
