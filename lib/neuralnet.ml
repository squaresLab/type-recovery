let initialize =
  let version = Some 3 in
  let minor = Some 6 in
  Py.initialize ?version:version ?minor:minor ()

let test_dynet () =
  let import_sys =
    "import sys\n\
     if not hasattr(sys, 'argv'):\n\
     \tsys.argv = ['']"
  in
  ignore (Py.Run.simple_string import_sys);
  Py.Run.simple_string "import dynet"
