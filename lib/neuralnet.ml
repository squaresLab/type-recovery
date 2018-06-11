let initialize =
  Py.initialize ~version:3 ~minor:6 ()

let py_mean (values : int list) =
  let m = Py.Import.add_module "ocaml" in
  let numbers = Py.List.of_list_map Py.Int.of_int values in
  Py.Module.set m "nums" numbers;
  let python_file = Py.Filename "lib/pynn/neuralnet.py" in
  Py.Run.simple_file python_file ""


let py_mean_return (values : int list) : float =
  let module_name = "lib.pynn.neuralnet" in
  let m = match Py.Import.try_import_module module_name with
  | Some m -> m
  | _ -> failwith (Printf.sprintf "Could not import %s" module_name) in
  let mean_python = Py.Module.get m "mean" in
  let mean = Py.Callable.to_function mean_python in
  let args = [| Py.List.of_list_map Py.Int.of_int values |] in
  Py.Float.to_float (mean args)

let test_python () =
  let values = [1; 2; 3; 4; 5; 6] in
  let mean = py_mean_return values in
  Printf.printf "%g\n" mean
