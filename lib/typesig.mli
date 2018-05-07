type memory = | Data of int
              | Padding of int

type tsig = memory list

val sigToStr : tsig -> string
val typeToOffsets : Cil.typ -> tsig
val addType : tsig -> string -> (tsig, string list) Hashtbl.t -> unit
val getTypenames : tsig -> (tsig, string list) Hashtbl.t -> string list
val getAltTypes : tsig -> (tsig, string list) Hashtbl.t -> string list list
