open Cil
open Pretty
open Lib.Utils

module E = Errormsg

let () =
  let fname = Sys.argv.(1) in
  let parsed = parseOneFile fname in
  iterGlobals parsed (fun glob -> E.log "Global: %a\n" d_global glob)
