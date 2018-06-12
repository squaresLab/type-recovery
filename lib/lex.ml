open Pyutils

let py_lex = get_module "python.lex"

let tokenize file_name =
  let py_file_name = Py.String.of_string file_name in
  let tokenize = get_python_fun py_lex "tokenize" in
  let tokens = tokenize [| py_file_name |] in
  Py.List.to_list_map Py.String.to_string tokens
