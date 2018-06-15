open Pyutils

let py_neuralnet = ref Py.none

let init vocab () =
  let ocaml_module = Py.Import.add_module "ocaml" in
  let py_vocab = Py.List.of_list_map Py.String.of_string vocab in
  Py.Module.set ocaml_module "vocab" py_vocab;
  py_neuralnet := get_module "python.neuralnet"

let py_train srnn params_srnn sentence =
  let sentence = Py.String.of_string sentence in
  let train = get_python_fun !py_neuralnet "train" in
  let args = [| srnn; params_srnn; sentence |] in
  train args

let test_dynet () =
  let sentence = "a quick brown fox jumped over the lazy dog" in
  let get = Py.Module.get !py_neuralnet in
  let srnn = get "srnn" in
  let params = get "params_srnn" in
  ignore (py_train srnn params sentence)
