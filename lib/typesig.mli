type memory = | Data of int
              | Padding of int

type tsig = memory list
type sigmap = (tsig, string list) Hashtbl.t

val signatures : sigmap

val string_of_sig : tsig -> string
val signature_id : tsig -> int
val offsets_of_type : Cil.typ -> tsig
val add_type : tsig -> string -> unit
val get_type_names : tsig -> string list
val get_alt_types : tsig -> string list list
val print_types : unit -> unit
val to_file : string -> unit
val from_file : string -> unit
