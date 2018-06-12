open Pyutils

let py_neuralnet = get_module "python.neuralnet"

let py_train srnn params_srnn sentence =
  let sentence = Py.String.of_string sentence in
  let train = get_python_fun py_neuralnet "train" in
  let args = [| srnn; params_srnn; sentence |] in
  train args

let test_dynet () =
  let sentence = "a quick brown fox jumped over the lazy dog" in
  let get = Py.Module.get py_neuralnet in
  let srnn = get "srnn" in
  let params = get "params_srnn" in
  ignore (py_train srnn params sentence)
