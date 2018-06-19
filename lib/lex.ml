open Cparser
open Utils

let vocab : was_seen = Hashtbl.create 3
let save_vocab filename = save_was_seen vocab filename
let load_vocab filename = load_was_seen vocab filename

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

let tokenize filename =
  let lexbuf = Clexer.init filename in
  let defs = Cparser.interpret Clexer.initial lexbuf in
  defs
  (* let collect_tokens infile =
   *   let rec helper tokens =
   *     match Clexer.initial infile with
   *     | EOF -> tokens
   *     | t -> begin
   *         let token_name = string_of_token t in
   *         Hashtbl.replace vocab token_name true;
   *         helper (token_name :: tokens)
   *       end
   *   in
   *   let tokens = List.rev (helper []) in
   *   Clexer.finish ();
   *   tokens
   * in
   * collect_tokens infile *)

class replace_types_visitor (type_names : string list) = object(self)
  inherit Cabsvisit.nopCabsVisitor
  method vspec spec =
    let open Cabs in
    let placeholder = Tnamed "<???>" in
    let replace = function
      | SpecType Tint -> SpecType placeholder
      | SpecType (Tnamed name) when List.mem name type_names ->
         SpecType placeholder
      | x -> x
    in
    ChangeTo (List.map replace spec)
end

let tokenize_replace_types type_names filename =
  let lexbuf = Clexer.init filename in
  let defs = Cparser.interpret Clexer.initial lexbuf in
  let redefine def =
    let visitor = new replace_types_visitor type_names in
    Cabsvisit.visitCabsDefinition visitor def
  in
  let modified_defs =
    List.fold_left (fun defs def -> List.append defs (redefine def)) [] defs in
  List.iter (fun d -> Cprint.print_def d; Printf.printf "\n") modified_defs
