open Cil
open Typesig

let main() =
  initCIL ();
  add_base_types global_signatures;
;;

main ();
exit 0
