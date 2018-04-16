open Lib.Utils

let () =
  let x = One in
  let word = match x with
    | Zero -> "Zero"
    | One  -> "One"
    | Two  -> "Two"
  in
  print_endline word
