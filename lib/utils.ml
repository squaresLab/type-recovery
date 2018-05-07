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

(* Partitions a list into a list of lists of all contiguous nonempty subsequences
 * e.g.:
 * listPartitions [1;2;3] = [[[1];[2];[3]]; [[1];[2;3]]; [[1;2];[3]]; [[1;2;3]]]*)
let rec listPartitions l =
  match l with
  | [] -> [[]]
  | x::xs ->
     let p = listPartitions xs in
     let fst = List.map (fun elem -> [x]::elem) p in
     match p with
     | [[]] -> fst
     | _    -> let rest = List.map (fun elem ->
                              match elem with
                              | ys::yss -> (x::ys)::yss
                              | _ -> []
                            ) p in
               fst @ rest
