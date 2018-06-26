open Utils

let get_file_hash fname =
  Digest.to_hex (Digest.file fname)

module Make () = struct
  let seen_files : was_seen = Hashtbl.create 3

  let add_file fhash = Hashtbl.replace seen_files fhash true

  let saw_file fhash = Hashtbl.mem seen_files fhash

  let to_sexp = was_seen_to_sexp seen_files

  let of_sexp sexp = was_seen_of_sexp seen_files sexp

  let to_file fname = was_seen_to_file seen_files fname

  let from_file fname = was_seen_from_file seen_files fname
end
