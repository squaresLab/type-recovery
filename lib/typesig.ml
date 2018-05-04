open Utils

type type_sig = int list [@@ deriving sexp]

module TypeMap = Map.Make(struct type t = type_sig let compare = compare end)

let addType type_sig name tbl =
  let cur_types = Hashtbl.find_opt tbl type_sig in
  match cur_types with
  | None -> Hashtbl.replace tbl type_sig [name]
  | Some ts -> Hashtbl.replace tbl type_sig (name::ts)

let getAltTypes type_sig tbl =
  match Hashtbl.find_opt tbl type_sig with
  | None -> "None"
  | Some ts -> strListToStr ts
