Py.initialize ~version:3 ~minor:6 ()

let get_module module_name =
  match Py.Import.try_import_module module_name with
  | Some m -> m
  | None -> failwith (Printf.sprintf "Could not import %s" module_name)

let get_python_fun python_module name =
  let python_function = Py.Module.get python_module name in
  Py.Callable.to_function python_function
