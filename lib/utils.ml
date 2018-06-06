open Cil

let string_of_list f l =
  let append cur next = Printf.sprintf "%s, %s" cur (f next) in
  match l with
  | fst::[] -> f fst
  | fst::rest -> List.fold_left append (f fst) rest
  | [] -> ""

let string_of_string_list l = string_of_list (fun x -> x) l

let string_of_int_list l = string_of_list string_of_int l

let string_of_file fname : string =
  let b = Buffer.create 255 in
  let fin = open_in fname in
  begin
    try
      while true do
        let line = input_line fin in
        Buffer.add_string b line;
        Buffer.add_char b '\n';
      done;
    with End_of_file -> close_in fin
  end;
  Buffer.contents b

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
    | _ -> helper (elem::acc) (n-1) in
  helper [] n

(* Partitions a list into a list of lists of all contiguous nonempty subsequences
 *
 * list_partitions [1;2;3] =
 *   [[[1];[2];[3]]; [[1];[2;3]]; [[1;2];[3]]; [[1;2;3]]]*)
let rec list_partitions = function
  | [] -> [[]]
  | x :: xs ->
     let p = list_partitions xs in
     let fst = List.map (fun elem -> [x]::elem) p in
     let rest = function
       | ys :: yss -> (x :: ys) :: yss
       | _ -> [] in
     match p with
     | [[]] -> fst
     | _ -> fst @ (List.map rest p)

let parse_one_file (fname : string) : file = Frontc.parse fname ()
