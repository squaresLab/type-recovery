open Cparser
open Utils

let input_vocab : was_seen = Hashtbl.create 3
let output_vocab : was_seen = Hashtbl.create 3

let save_vocabs filename =
  was_seen_to_file input_vocab ("input-" ^ filename);
  was_seen_to_file output_vocab ("output-" ^ filename)

let load_vocabs filename =
  was_seen_from_file input_vocab ("input-" ^ filename);
  was_seen_from_file output_vocab ("output-" ^ filename)

let string_of_token = function
  | IDENT (s, _) | QUALIFIER (s, _) -> s
  | CST_CHAR _ | CST_WCHAR _ -> "<CHAR_CONST>"
  | CST_INT _ -> "<INT_CONST>"
  | CST_FLOAT _ -> "<FLOAT_CONST>"
  | CST_STRING _ | CST_WSTRING _ -> "<STR_CONST>"
  | NAMED_TYPE (s, _) -> s
  | CHAR _ -> "char"
  | INT _ -> "int"
  | BOOL _ -> "bool"
  | DOUBLE _ -> "double"
  | FLOAT _ -> "float"
  | VOID _ -> "void"
  | INT64 _ -> "__int64"
  | INT32 _ -> "__int32"
  | ENUM _ -> "enum"
  | STRUCT _ -> "struct"
  | TYPEDEF _ -> "typedef"
  | UNION _ -> "union"
  | SIGNED _ -> "signed"
  | UNSIGNED _ -> "unsigned"
  | LONG _ -> "long"
  | SHORT _ -> "short"
  | VOLATILE _ -> "volatile"
  | EXTERN _ -> "extern"
  | STATIC _ -> "static"
  | CONST _ -> "const"
  | RESTRICT _ -> "restrict"
  | AUTO _ -> "auto"
  | REGISTER _ -> "register"
  | THREAD _ -> "thread"
  | SIZEOF _ -> "sizeof"
  | ALIGNOF _ -> "alignof"
  | EQ -> "="
  | PLUS_EQ -> "+="
  | MINUS_EQ -> "-="
  | STAR_EQ -> "*="
  | SLASH_EQ -> "/="
  | PERCENT_EQ -> "%="
  | AND_EQ -> "&="
  | PIPE_EQ -> "|="
  | CIRC_EQ -> "^="
  | INF_INF_EQ -> "<<="
  | SUP_SUP_EQ -> ">>="
  | ARROW -> "->"
  | DOT -> "."
  | EQ_EQ -> "=="
  | EXCLAM_EQ -> "!="
  | INF -> "<"
  | SUP -> ">"
  | INF_EQ -> "<="
  | SUP_EQ -> ">="
  | PLUS _ -> "+"
  | MINUS _ -> "-"
  | STAR _ -> "*"
  | SLASH -> "/"
  | PERCENT -> "%"
  | TILDE _ -> "~"
  | AND _ -> "&"
  | PIPE -> "|"
  | CIRC -> "^"
  | EXCLAM _ -> "!"
  | AND_AND _ -> ""
  | PIPE_PIPE -> "||"
  | INF_INF -> "<<"
  | SUP_SUP -> ">>"
  | PLUS_PLUS _ -> "++"
  | MINUS_MINUS _ -> "--"
  | RPAREN -> ")"
  | LPAREN _ -> "("
  | RBRACE _ -> "}"
  | LBRACE _ -> "{"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | COLON -> ":"
  | SEMICOLON _ -> ";"
  | COMMA -> ","
  | ELLIPSIS -> "..."
  | QUEST -> "?"
  | BREAK _ -> "break"
  | CONTINUE _ -> "continue"
  | GOTO _ -> "goto"
  | RETURN _ -> "return"
  | SWITCH _ -> "switch"
  | CASE _ -> "case"
  | DEFAULT _ -> "default"
  | WHILE _ -> "while"
  | DO _ -> "do"
  | FOR _ -> "for"
  | IF _ -> "if"
  | TRY _ -> "try"
  | EXCEPT _ -> "except"
  | FINALLY _ -> "finally"
  | ELSE -> "else"
  | ATTRIBUTE _ -> "attribute"
  | INLINE _ -> "inline"
  | ASM _ -> "__asm"
  | TYPEOF _ -> "typeof"
  | FUNCTION__ _ -> "__FUNCTION__"
  | PRETTY_FUNCTION__ _ -> "__PRETTY_FUNCTION__"
  | LABEL__ -> "LABEL"
  | BUILTIN_VA_ARG _ -> "__builtin_va_arg"
  | ATTRIBUTE_USED _ -> "__attribute((used))"
  | BUILTIN_VA_LIST -> "__builtin_va_list"
  | BLOCKATTRIBUTE -> "blockattribute"
  | BUILTIN_TYPES_COMPAT _ -> "__builtin_types_compat"
  | BUILTIN_OFFSETOF _ -> "__builtin_types_compat"
  | DECLSPEC _ -> "DECLSPEC"
  | MSASM (s, _) | MSATTR (s, _) | PRAGMA_LINE (s, _) -> s
  | PRAGMA _ -> "__pragma"
  | PRAGMA_EOL -> ""
  | AT_TRANSFORM _ -> "@transform"
  | AT_TRANSFORMEXPR _ -> "@transformExpr"
  | AT_SPECIFIER _ -> "@specifier"
  | AT_EXPR _ -> "@expr"
  | AT_NAME -> "@name"
  | EOF -> ""

class strip_constants_visitor = object(self)
  inherit Cabsvisit.nopCabsVisitor
  method! vexpr = function
    | CONSTANT (CONST_INT _) -> ChangeTo (CONSTANT (CONST_INT "<int>"))
    | CONSTANT (CONST_FLOAT _) -> ChangeTo (CONSTANT (CONST_FLOAT "<float>"))
    | CONSTANT (CONST_CHAR _) -> ChangeTo (CONSTANT (CONST_CHAR [Int64.zero]))
    | CONSTANT (CONST_WCHAR _) -> ChangeTo (CONSTANT (CONST_WCHAR [Int64.zero]))
    | CONSTANT (CONST_STRING _) ->
       ChangeTo (CONSTANT (CONST_STRING "<string>"))
    | CONSTANT (CONST_WSTRING _) ->
       ChangeTo (CONSTANT (CONST_WSTRING [Int64.zero]))
    | _ -> DoChildren
end

(* FIXME: This currently only works with [32] types. Some thought will have to
   go into extending this *)
class replace_types_visitor (type_names : string list) = object(self)
  inherit Cabsvisit.nopCabsVisitor
  val mutable correct_vals = []
  method! vspec spec =
    let open Cabs in
    let placeholder = SpecType (Tnamed "<???>") in
    let long = SpecType Tlong in
    let short = SpecType Tshort in
    if List.mem long spec || List.mem short spec then
      DoChildren
    else begin
        let find_type = function
          | SpecType Tint -> true
          | SpecType (Tnamed name) when List.mem name type_names -> true
          | _ -> false
        in
        if List.exists find_type spec then begin
            let collect (collected, rest) specifier =
              match specifier with
              | SpecType _ ->
                 let collected = List.rev (specifier :: collected) in
                 (collected, rest)
              | _ ->
                 let rest = List.rev (specifier :: rest) in
                 (collected, rest)
            in
            let collected, rest = List.fold_left collect ([], []) spec in
            correct_vals <- List.rev (collected :: correct_vals);
            ChangeTo (List.rev (placeholder :: rest))
          end
        else
          DoChildren
      end
  method get_correct_vals = correct_vals
end

let tokenize filename =
  let lexbuf = Clexer.init filename in
  let defs = Cparser.interpret Clexer.initial lexbuf in
  Clexer.finish ();
  defs

(* This tokenizes files for token-level NN models. It does the following:
 * - Strip constants
 * - Replace [32] types with <???> tokens
 * - Collect the corresponding type definitions for those holes
 * - Generate a list of pairs. Each pair is an input and its corresponding
 *   output tag. The input is either a token or a hole, the output is either
 *   a string corresponding to a type definition or an empty token: <--->
 *)
let tokenize_training_pairs type_names filename =
  let lexbuf = Clexer.init filename in
  let defs = Cparser.interpret Clexer.initial lexbuf in
  Clexer.finish ();
  let strip_constants defs def =
    let visitor = new strip_constants_visitor in
    let stripped_def =
      Cabsvisit.visitCabsDefinition (visitor :> Cabsvisit.cabsVisitor) def
    in
    defs @ stripped_def
  in
  let stripped_defs = List.fold_left strip_constants [] defs in

  let replace_visitor = new replace_types_visitor type_names in
  let replace_types defs def =
    let redefined_def =
      Cabsvisit.visitCabsDefinition
        (replace_visitor :> Cabsvisit.cabsVisitor) def
    in
    defs @ redefined_def
  in
  let modified_defs = List.fold_left replace_types [] stripped_defs in

  let correct_vals = replace_visitor#get_correct_vals in
  let collect f collected item =
    f item;
    let item_string = Cprint.Sprint.get_string () in
    List.rev (item_string :: collected)
  in
  let collect_specifiers = collect Cprint.Sprint.print_specifiers in
  let collect_def = collect Cprint.Sprint.print_def in

  (* List of strings corresponding to the correct type definitions, in order *)
  let correct_strings =
    ref (List.fold_left collect_specifiers [] correct_vals)
  in
  List.iter (fun c -> Hashtbl.replace output_vocab c true) !correct_strings;

  (* List of strings corresponding to each line of the code after constant
   * removal and hole insertion. This needs to be flattened into a list of
   * tokens. *)
  let defs_strings = List.fold_left collect_def [] modified_defs in
  let add_definition tokens def =
    let split_def = Str.split (Str.regexp " ") def in
    tokens @ split_def
  in
  let tokenized_file = List.fold_left add_definition [] defs_strings in
  List.iter (fun tok -> Hashtbl.replace input_vocab tok true) tokenized_file;
  let create_pair next_token =
    match next_token with
    | "<???>" -> begin
        match !correct_strings with
        | next :: rest ->
           correct_strings := rest;
           (next_token, next)
        | [] -> failwith "Error, not enough correct strings"
      end
    | _ -> (next_token, "<--->")
  in
  List.map create_pair tokenized_file
