open Pyutils
open Cparser

let py_lex = get_module "python.lex"

(* let tokenize file_name =
 *   let py_file_name = Py.String.of_string file_name in
 *   let tokenize = get_python_fun py_lex "tokenize" in
 *   let tokens = tokenize [| py_file_name |] in
 *   Py.List.to_list_map Py.String.to_string tokens *)

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
  | ASM _ -> "asm"
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
  | PRAGMA _ -> "pragma"
  | PRAGMA_EOL -> ""
  | AT_TRANSFORM _ -> ""
  | AT_TRANSFORMEXPR _ -> ""
  | AT_SPECIFIER _ -> ""
  | AT_EXPR _ -> ""
  | AT_NAME -> ""
  | EOF -> ""

let tokenize filename =
  let infile = Clexer.init filename in
  let collect_tokens infile =
    let tokens = ref [] in
    let finished = ref false in
    while not !finished do
      match Clexer.initial infile with
      | EOF -> finished := true
      | x -> tokens := x :: !tokens
    done;
    Clexer.finish ();
    List.rev !tokens
  in
  let collected_tokens = collect_tokens infile in
  List.map string_of_token collected_tokens
