open Cil
open Pretty
open Lib.Utils
open Lib.Types
open Lib.Typesig

module E = Errormsg

let signatures = Hashtbl.create 3

let dispAltTypes types =
  E.log "Alternate types: %s\n" (List.fold_left (fun cur typelist ->
                                     cur ^ "[" ^ (strListToStr typelist) ^ "] "
                                   ) "" types)

let funInfo glob =
  match glob with
  | GFun (f, loc) ->
     let return_type =
       match f.svar.vtype with
       | TFun (t, _, _, _) -> t
       | _ -> voidType
     in
     let alt_types = getAltTypes (typeToOffsets return_type) signatures in
     E.log "Function: %s has return type %a\n" f.svar.vname d_type return_type;
     dispAltTypes alt_types;
     let formals_sig = List.fold_left (fun sigs formal ->
                           let formal_sig = typeToOffsets formal.vtype in
                           sigs@formal_sig
                         ) [] f.sformals
     in
     let locals_sig = List.fold_left (fun sigs local ->
                          let local_sig = typeToOffsets local.vtype in
                          sigs@local_sig
                        ) [] f.slocals
     in
     let formal_alts = getAltTypes formals_sig signatures in
     let local_alts = getAltTypes locals_sig signatures in
     E.log "Formal types: [%s]\n" (listToString
                                     (fun f ->
                                       sprint 10 (dprintf "%a" d_type f.vtype)
                                     ) f.sformals);
     E.log "Formal sig: [%s]\n" (intListToStr formals_sig);
     dispAltTypes formal_alts;
     E.log "Local types: [%s]\n" (listToString
                                    (fun f ->
                                      sprint 10 (dprintf "%a" d_type f.vtype)
                                    ) f.slocals);
     E.log "Local sig: [%s]\n" (intListToStr locals_sig);
     E.log "Alternate local types: %s\n" (List.hd (List.hd local_alts)) (* FIXME *)
  | _ -> ()

let addBaseTypes () =
  List.iter (fun t ->
      let type_sig = typeToOffsets t in
      addType type_sig (string_of_type t) signatures
    ) (baseTypes @ basePointerTypes)

let collectTypes glob =
  match glob with
  | GType (t, _) ->
     let type_sig = typeToOffsets t.ttype in
     addType type_sig t.tname signatures
  | GCompTag (cinfo, _) ->
     let ttype = TComp (cinfo, []) in
     let type_sig = typeToOffsets ttype in
     addType type_sig cinfo.cname signatures
  (* Enums probably need to be treated differently *)
  | GEnumTag (einfo, _) ->
     let ttype = TEnum (einfo, []) in
     let type_sig = typeToOffsets ttype in
     addType type_sig einfo.ename signatures
   | _ -> ()

let printTypes sigs =
  Hashtbl.iter (fun type_sig type_names ->
      E.log "Types with signature [%s]: %s\n"
        (intListToStr type_sig)
        (strListToStr type_names)
    ) sigs

let main () =
  initCIL ();
  let fname = Sys.argv.(1) in
  let parsed = parseOneFile fname in
  addBaseTypes ();
  iterGlobals parsed collectTypes;
  iterGlobals parsed (fun f ->
      match f with
      | GFun _ -> funInfo f; E.log "\n"
      | _ -> ()
    );
  printTypes signatures
;;

main ();
exit 0
