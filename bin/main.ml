open Cil
open Pretty
open Lib.Utils
open Lib.Types

module E = Errormsg

let tmap = ref TypeMap.empty

let typeToOffsets t =
  match t with
  | TArray (base_type, exp, attrs) ->
     let base_type_size = bitsSizeOf base_type in
     begin
       match exp with
       | None -> []
       | Some e ->
          begin
            match isInteger e with
            | None -> E.log "Expression is not an integer\n"; []
            | Some i -> repeat (i64_to_int i) base_type_size
          end
     end
  | TComp (cinfo, _) when cinfo.cstruct ->
     let alignBits = 8 * (alignOf_int t) in
     let (info, _) =
       List.fold_left (fun (offsets, curAlign) field ->
           let tsize = bitsSizeOf field.ftype in
           let nextAlign =
             match tsize mod alignBits with
             | 0 -> 0
             | t -> alignBits - t
           in
           match curAlign with
           | 0 -> (tsize::offsets, nextAlign)
           | c ->
              begin
                match field.ftype with
                | TInt (IChar, _) ->
                   (* If Char don't add padding *)
                   (8::offsets, c - 8)
                | t ->
                   (* Else pad if needed *)
                   (tsize::c::offsets, nextAlign)
              end
         ) ([], 0) cinfo.cfields
     in
     List.rev info
  | _ -> [bitsSizeOf t]

let altTypes t =
  let t_sig = typeToOffsets t in
  let alts = TypeMap.find_opt t_sig !tmap in
  match alts with
  | None -> "None"
  | Some ts -> strListToStr ts

let funInfo glob =
  match glob with
  | GFun (f, loc) ->
     let return_type =
       match f.svar.vtype with
       | TFun (t, _, _, _) -> t
       | _ -> voidType
     in
     E.log "Function: %s has return type %a\n" f.svar.vname d_type return_type;
     E.log "Alternate types: %s\n" (altTypes return_type);
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
     let formal_alts = getAltTypes formals_sig tmap in
     let local_alts = getAltTypes locals_sig tmap in
     E.log "Formal types: [%s]\n" (listToString
                                     (fun f ->
                                       sprint 10 (dprintf "%a" d_type f.vtype)
                                     ) f.sformals);
     E.log "Formal sig: [%s]\n" (intListToStr formals_sig);
     E.log "Alternate formal types: %s\n" formal_alts;
     E.log "Local types: [%s]\n" (listToString
                                    (fun f ->
                                      sprint 10 (dprintf "%a" d_type f.vtype)
                                    ) f.slocals);
     E.log "Local sig: [%s]\n" (intListToStr locals_sig);
     E.log "Alternate local types: %s\n" local_alts
  | _ -> ()

let addBaseTypes () =
  List.iter (fun t ->
      let type_sig = typeToOffsets t in
      addToMap type_sig (string_of_type t) tmap
    ) (baseTypes @ basePointerTypes)

let collectTypes glob =
  match glob with
  | GType (t, _) ->
     let type_sig = typeToOffsets t.ttype in
     addToMap type_sig t.tname tmap
  | GCompTag (cinfo, _) ->
     let ttype = TComp (cinfo, []) in
     let type_sig = typeToOffsets ttype in
     addToMap type_sig cinfo.cname tmap
  (* Enums probably need to be treated differently *)
  | GEnumTag (einfo, _) ->
     let ttype = TEnum (einfo, []) in
     let type_sig = typeToOffsets ttype in
     addToMap type_sig einfo.ename tmap
   | _ -> ()

let printTypes type_map =
  if TypeMap.is_empty type_map then
    E.log "Empty!\n";
  TypeMap.iter (fun type_sig type_names ->
      E.log "Types with signature [%s]: %s\n"
        (intListToStr type_sig)
        (strListToStr type_names)
    ) type_map

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
  printTypes !tmap
;;

main ();
exit 0
