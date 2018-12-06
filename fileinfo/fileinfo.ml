open Utils

let get_file_hash fname = Digest.to_hex (Digest.file fname)

let empty : was_seen =  Hashtbl.create 3

let add_file info fhash = Hashtbl.replace info fhash true

let saw_file info fhash = Hashtbl.mem info fhash

let to_sexp info = was_seen_to_sexp info

let from_sexp info sexp = was_seen_from_sexp info sexp

let to_file info fname = was_seen_to_file info fname

let from_file info fname = was_seen_from_file info fname
