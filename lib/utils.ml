open Cil

let parseOneFile (fname : string) : file =
  let cil = Frontc.parse fname () in
  cil
