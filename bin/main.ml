open Cil
open Pretty
open Lib.Utils

module E = Errormsg

module TypeMap = Map.Make(struct type t = int let compare = compare end)

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
  | TComp (cinfo, _) ->
     begin
       if cinfo.cstruct then
         (* Structs *)
         begin
           let alignment_bits = 8 * (alignOf_int t) in
           List.fold_left (fun typeList field ->
             (* Compute the size of the field and any needed padding *)
               []
             ) [] cinfo.cfields
         end
       else
         (* Unions *)
         [bitsSizeOf t]
     end
  | _ -> []

let doGlobal glob =
  match glob with
  | GFun (f, loc)   -> printFunLocals f
  | GVar (v, _, _)  -> E.log "Global: %a\n" d_varinfo v
  | GCompTag (c, _) -> E.log "Global: %s\n" c.cname
  | GCompTagDecl (c, _) -> E.log "Global decl: %s\n" c.cname
  | GType (t, _)    ->
     begin
       let this_type_info =
         match t.ttype with
         | TComp _ | TArray _ -> "composite type"
         | _        -> "not composite type"
       in
       E.log "Alignment for type %s: %d bytes\n" t.tname (alignOf_int t.ttype);
       let type_size = bitsSizeOf t.ttype in
       E.log "Type offsets: [%s]\n" (intListToStr (typeToOffsets t.ttype));
       let cur_types = TypeMap.find_opt type_size !tmap in
       begin
         match cur_types with
         | None ->
            tmap := TypeMap.add type_size [t.tname] !tmap
         | Some ts ->
            tmap := TypeMap.add type_size (t.tname::ts) !tmap
       end;
       E.log "Global type: %s : %a : %d bits : %s\n" t.tname d_type t.ttype (bitsSizeOf t.ttype) this_type_info
     end
  (* | GVarDecl (v, _) -> E.log "Global: %a\n" d_varinfo v *)
  | _ -> ()

let printTypes type_map =
  if TypeMap.is_empty type_map then
    E.log "Empty!\n";
  TypeMap.iter (fun type_size type_names ->
      E.log "Types with size %d bits: %s\n" type_size (strListToStr type_names)
    ) type_map

let main () =
  initCIL ();
  let fname = Sys.argv.(1) in
  let parsed = parseOneFile fname in
  iterGlobals parsed doGlobal;
  printTypes !tmap
;;

begin
  try
    main ()
  with
  | _ -> ()
end;
exit 0
