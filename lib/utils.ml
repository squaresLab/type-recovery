open Cil

module TypeMap = Map.Make(struct type t = int list let compare = compare end)

let listToString f l =
  match l with
  | [] -> ""
  | fst::[] -> f fst
  | fst::rest ->
     List.fold_left (fun cur next -> Printf.sprintf "%s, %s" cur (f next)) (f fst) rest

let strListToStr l =
  listToString (fun x -> x) l

let intListToStr l =
  listToString string_of_int l

let repeat n elem =
  let rec helper acc n =
    match n with
    | 0 -> acc
    | _ -> helper (elem::acc) (n-1)
  in
  helper [] n

let parseOneFile (fname : string) : file =
  let cil = Frontc.parse fname () in
  cil

let addToMap type_sig name map =
  let cur_types = TypeMap.find_opt type_sig !map in
  match cur_types with
  | None -> map := TypeMap.add type_sig [name] !map
  | Some ts -> map := TypeMap.add type_sig (name::ts) !map

let getAltTypes type_sig map =
  match TypeMap.find_opt type_sig !map with
  | None -> "None"
  | Some ts -> strListToStr ts
