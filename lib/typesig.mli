type memory = | Data of int
              | Padding of int

type tsig = memory list
type sigmap = (tsig, string list) Hashtbl.t
type filemap = (string, sigmap) Hashtbl.t

val global_signatures : sigmap
val file_signatures : filemap


val string_of_sig : tsig -> string
val offsets_of_type : Cil.typ -> tsig
val collect_types : Cil.file -> unit
val add_base_types : unit -> unit
val get_type_names : sigmap -> tsig -> string list
val get_alt_types : sigmap -> tsig -> string list list
val print_types : sigmap -> unit
val to_file : string -> unit
val from_file : string -> unit
