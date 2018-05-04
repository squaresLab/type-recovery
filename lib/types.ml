(* Additional base types not defined by CIL *)
open Cil

let scharType      = TInt(ISChar, [])
let ucharType      = TInt(IUChar, [])
let boolType       = TInt(IBool,  [])
let shortType      = TInt(IShort, [])
let ushortType     = TInt(IUShort, [])
let longlongType   = TInt(ILongLong, [])
let ulonglongType  = TInt(IULongLong, [])
let floatType      = TFloat(FFloat, [])
let longdoubleType = TFloat(FLongDouble, [])

let rec string_of_type t =
  match t with
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
  | TPtr (t, _) -> "* " ^ (string_of_type t)
  | TArray (t, exp, _) -> (string_of_type t) ^ "[]"
  | _ -> ""

let intTypes = [charType;
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
                ulonglongType;
  ]

let floatTypes = [floatType; doubleType; longdoubleType;]

let baseTypes = [voidType] @ intTypes @ floatTypes
