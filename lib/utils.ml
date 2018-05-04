open Cil

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
