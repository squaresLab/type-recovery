type memory = | Data of int
              | Padding of int

type tsig = memory list

val sigToStr : tsig -> string
val typeToOffsets : Cil.typ -> tsig
val addType : tsig -> string -> unit
val getTypenames : tsig -> string list
val getAltTypes : tsig -> string list list
val printTypes : unit -> unit
