val scharType : Cil.typ
val ucharType : Cil.typ
val boolType : Cil.typ
val shortType : Cil.typ
val ushortType : Cil.typ
val longlongType : Cil.typ
val ulonglongType : Cil.typ
val floatType : Cil.typ
val longdoubleType : Cil.typ

val string_of_type : Cil.typ -> string
val int_types : Cil.typ list
val float_types : Cil.typ list
val base_types : Cil.typ list
val add_pointers_to : Cil.typ list -> Cil.typ list
