open Cil
open Utils

type tsig = int list [@@ deriving sexp]

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
            | None -> failwith "Expression is not an integer\n"
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

let addType type_sig name tbl =
  let cur_types = Hashtbl.find_opt tbl type_sig in
  match cur_types with
  | None -> Hashtbl.replace tbl type_sig [name]
  | Some ts -> Hashtbl.replace tbl type_sig (name::ts)

let getTypenames type_sig tbl =
  match Hashtbl.find_opt tbl type_sig with
  | None -> ["None"]
  | Some ts -> ts

let getAltTypes type_sig tbl =
  let signature_partitions =
    List.filter
      (List.for_all (fun s -> Hashtbl.mem tbl s)) (listPartitions type_sig) in
  List.fold_left (fun type_lists part ->
      let part_types = List.map (fun s -> Hashtbl.find tbl s) part in
      (product part_types)@type_lists
    ) [] signature_partitions
