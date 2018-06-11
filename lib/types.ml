(* Additional base types not defined by CIL *)
open Cil
open Utils

(* Naming convention to match CIL *)
let scharType      = TInt(ISChar, [])
let ucharType      = TInt(IUChar, [])
let boolType       = TInt(IBool,  [])
let shortType      = TInt(IShort, [])
let ushortType     = TInt(IUShort, [])
let longlongType   = TInt(ILongLong, [])
let ulonglongType  = TInt(IULongLong, [])
let floatType      = TFloat(FFloat, [])
let longdoubleType = TFloat(FLongDouble, [])

let rec string_of_type = function
  | TVoid _     -> "void"
  | TInt (i, _) ->
     begin
       match i with
       | IChar      -> "char"
       | ISChar     -> "signed char"
       | IUChar     -> "unsigned char"
       | IBool      -> "_Bool"
       | IInt       -> "int"
       | IUInt      -> "unsigned int"
       | IShort     -> "short"
       | IUShort    -> "unsigned short"
       | ILong      -> "long"
       | IULong     -> "unsigned long"
       | ILongLong  -> "long long"
       | IULongLong -> "unsigned long long"
     end
  | TFloat (f, _) ->
     begin
       match f with
       | FFloat      -> "float"
       | FDouble     -> "double"
       | FLongDouble -> "long double"
     end
  | TPtr (t, _) ->
     begin
       match t with
       | TPtr _ -> "*"  ^ (string_of_type t)
       | _      -> "* " ^ (string_of_type t)
     end
  | TArray (t, exp, _) -> (string_of_type t) ^ "[]"
  | _ -> ""

let string_of_exp exp =
  let exp_doc = Pretty.dprintf "%a" d_exp exp in
  Pretty.sprint 80000 exp_doc

let rec string_of_offset = function
  | NoOffset -> ""
  | Field (fieldinfo, offset) ->
     let next_offset = string_of_offset offset in
     Printf.sprintf ".%s%s" fieldinfo.fname next_offset
  | Index (exp, offset) ->
     let exp_string = string_of_exp exp in
     let next_offset = string_of_offset offset in
     Printf.sprintf "[%s]%s" exp_string next_offset

let rec string_of_lval = function
  | (Var (v), offset) ->
     let type_string = string_of_type v.vtype in
     let offset_string = string_of_offset offset in
     Printf.sprintf "<%s> %s%s" type_string v.vname offset_string
  | (Mem (exp), offset) ->
     "*" ^ string_of_exp exp

let int_types = [charType;
                 scharType;
                 ucharType;
                 boolType;
                 intType;
                 uintType;
                 shortType;
                 ushortType;
                 longType;
                 ulongType;
                 longlongType;
                 ulonglongType;]

let float_types = [floatType; doubleType; longdoubleType;]

let base_types = [voidType] @ int_types @ float_types

let add_pointers_to types =
  let add_pointer ptr t = (TPtr (t, [])) :: ptr in
  List.fold_left add_pointer [] types
