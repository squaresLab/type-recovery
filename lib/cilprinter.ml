open Cil
open Pretty
open Utils
module E = Errormsg

let output_cil_file out cilfile =
  let directive_style = !lineDirectiveStyle in
  lineDirectiveStyle := None;
  E.hadErrors := false;
  let remove_builtins cilfile =
    let is_builtin = function
      | GVarDecl(vi, _) when not !printCilAsIs
                             && Hashtbl.mem builtinFunctions vi.vname -> false
      | _ -> true in
    {cilfile with globals = List.filter is_builtin cilfile.globals} in
  begin
    try
      iterGlobals (remove_builtins cilfile) (dumpGlobal defaultCilPrinter out);
    with Errormsg.Error ->
      Printf.fprintf out "#error \"Cilprinter.output_cil_file failed!\"\n";
      flush out
  end;
  lineDirectiveStyle := directive_style

let print_cil_file cilfile = output_cil_file stdout cilfile

let string_of_cil_file cilfile =
  let fname, chan = Filename.open_temp_file "" ".c" in
  output_cil_file chan cilfile;
  close_out chan;
  let body = string_of_file fname in
  Sys.remove fname;
  body
