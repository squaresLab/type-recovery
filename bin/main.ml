open Cil
open Pretty
open Lib.Utils

module E = Errormsg

module TypeMap = Map.Make(struct type t = int list let compare = compare end)

let tmap = ref TypeMap.empty

let d_varinfo () v =
  let real_type = typeSig v.vtype in
  dprintf "%s has type signature %a" v.vname d_typsig real_type
  (* let real_type = match v.vtype with
   *   | TNamed (tinfo, _) -> tinfo.ttype
   *   | _ -> v.vtype
   * in
   * dprintf "%s : %a %d bits" v.vname d_type real_type (bitsSizeOf real_type) *)

let printFunLocals f =
  E.log "Function %a\n" d_varinfo f.svar;
  List.iter (fun v -> E.log "Formal: %a\n" d_varinfo v) f.sformals;
  List.iter (fun v -> E.log "Local: %a\n" d_varinfo v) f.slocals

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

let doGlobal glob =
  match glob with
  | GFun (f, loc)   -> printFunLocals f
  | GVar (v, _, _)  -> E.log "Global: %a\n" d_varinfo v
  | GCompTag (c, _) -> E.log "Global: %s\n" c.cname
  | GCompTagDecl (c, _) -> E.log "Global decl: %s\n" c.cname
  | _ -> ()

let alt_types t =
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
     E.log "Alternate types: %s\n" (alt_types return_type);
     let formals_sig = List.fold_left (fun sigs formal ->
                           let formal_sig = typeToOffsets formal.vtype in
                           sigs@formal_sig
                         ) [] f.sformals
     in
     E.log "Formal types: [%s]\n" (listToString
                                     (fun f ->
                                       sprint 10 (dprintf "%a" d_type f.vtype)
                                     ) f.sformals);
     E.log "Formal sig: [%s]\n" (intListToStr formals_sig);
     let formal_alts =
       match TypeMap.find_opt formals_sig !tmap with
       | None -> "None"
       | Some ts -> strListToStr ts
     in
     E.log "Alternate formal types: %s\n" formal_alts
  | _ -> ()

let collectTypes glob =
  match glob with
  | GType (t, _) ->
     let type_sig = typeToOffsets t.ttype in
     let cur_types = TypeMap.find_opt type_sig !tmap in
     begin
       match cur_types with
       | None -> tmap := TypeMap.add type_sig [t.tname] !tmap
       | Some ts -> tmap := TypeMap.add type_sig (t.tname::ts) !tmap
     end
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
  iterGlobals parsed collectTypes;
  iterGlobals parsed funInfo;
  printTypes !tmap
;;

main ();
exit 0
