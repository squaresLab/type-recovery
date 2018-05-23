open Cil

let list_to_string f l =
  let append cur next = Printf.sprintf "%s, %s" cur (f next) in
  match l with
  | fst::[] -> f fst
  | fst::rest -> List.fold_left append (f fst) rest
  | [] -> ""

let string_of_string_list l = list_to_string (fun x -> x) l

let string_of_int_list l = list_to_string string_of_int l

(* product [[1;2];[3;4];[5;6]] = [[1;3;5];[1;3;6];[1;4;5];[1;4;6]...[2;4;6]] *)
let rec product = function
  | hd::tl ->
     let helper cur elem =
       let next = List.map (fun x -> elem :: x) (product tl) in
       cur @ next in
     List.fold_left helper [] hd
  | _ -> [[]]

let repeat n elem =
  let rec helper acc n =
    match n with
    | 0 -> acc
    | _ -> helper (elem::acc) (n-1)
  in
  helper [] n

(* Partitions a list into a list of lists of all contiguous nonempty subsequences
 * e.g.:
 * list_partitions [1;2;3] = [[[1];[2];[3]]; [[1];[2;3]]; [[1;2];[3]]; [[1;2;3]]]*)
let rec list_partitions l =
  match l with
  | [] -> [[]]
  | x::xs ->
     let p = list_partitions xs in
     let fst = List.map (fun elem -> [x]::elem) p in
     match p with
     | [[]] -> fst
     | _    -> let rest = List.map (fun elem ->
                              match elem with
                              | ys::yss -> (x::ys)::yss
                              | _ -> []
                            ) p in
               fst @ rest

let parse_one_file (fname : string) : file =
  let cil = Frontc.parse fname () in
  cil
