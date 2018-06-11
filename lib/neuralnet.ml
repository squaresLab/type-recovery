let initialize =
  Py.initialize ~version:3 ~minor:6 ()

let test_dynet () =
  let import_sys =
    "import sys\n\
     if not hasattr(sys, 'argv'):\n\
     \tsys.argv = ['']"
  in
  ignore (Py.Run.simple_string import_sys);
  Py.Run.simple_string "import dynet"
