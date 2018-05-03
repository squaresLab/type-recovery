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
       List.fold_left (fun (typeList, curAlign) field ->
           let tsize = bitsSizeOf field.ftype in
           let nextAlign =
             match tsize mod alignBits with
             | 0 -> 0
             | t -> alignBits - t
           in
           match curAlign with
           | 0 -> (typeList@[tsize], nextAlign)
           | c ->
              begin
                match field.ftype with
                | TInt (IChar, _) ->
                   (* If Char don't add padding *)
                   (typeList@[8], c - 8)
                | t ->
                   (* Else pad if needed *)
                   (typeList@[c; tsize], nextAlign)
              end
         ) ([], 0) cinfo.cfields
     in
     info
  | _ -> [bitsSizeOf t]

let doGlobal glob =
  match glob with
  | GFun (f, loc)   -> printFunLocals f
  | GVar (v, _, _)  -> E.log "Global: %a\n" d_varinfo v
  | GCompTag (c, _) -> E.log "Global: %s\n" c.cname
  | GCompTagDecl (c, _) -> E.log "Global decl: %s\n" c.cname
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
      E.log "Types with signature [%s]: %s\n" (intListToStr type_sig) (strListToStr type_names)
    ) type_map

let main () =
  initCIL ();
  let fname = Sys.argv.(1) in
  let parsed = parseOneFile fname in
  iterGlobals parsed collectTypes;
  (* iterGlobals parsed doGlobal; *)
  printTypes !tmap
;;

begin
  try
    main ()
  with
  | _ -> ()
end;
exit 0
