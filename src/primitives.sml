(* This file was generated by primitives.lua *)
structure Primitives = struct
datatype PrimOp = PrimOp_EQUAL (* = *)
                | PrimOp_call2 (* call2 *)
                | PrimOp_call3 (* call3 *)
                | PrimOp_Ref_EQUAL (* Ref.= *)
                | PrimOp_Ref_set (* Ref.:= *)
                | PrimOp_Ref_read (* Ref.! *)
                | PrimOp_Bool_EQUAL (* Bool.= *)
                | PrimOp_Bool_not (* Bool.not *)
                | PrimOp_Int_EQUAL (* Int.= *)
                | PrimOp_Int_LT (* Int.< *)
                | PrimOp_Int_LE (* Int.<= *)
                | PrimOp_Int_GT (* Int.> *)
                | PrimOp_Int_GE (* Int.>= *)
                | PrimOp_Word_EQUAL (* Word.= *)
                | PrimOp_Word_PLUS (* Word.+ *)
                | PrimOp_Word_MINUS (* Word.- *)
                | PrimOp_Word_TIMES (* Word.* *)
                | PrimOp_Word_TILDE (* Word.~ *)
                | PrimOp_Word_LT (* Word.< *)
                | PrimOp_Word_LE (* Word.<= *)
                | PrimOp_Word_GT (* Word.> *)
                | PrimOp_Word_GE (* Word.>= *)
                | PrimOp_Real_PLUS (* Real.+ *)
                | PrimOp_Real_MINUS (* Real.- *)
                | PrimOp_Real_TIMES (* Real.* *)
                | PrimOp_Real_DIVIDE (* Real./ *)
                | PrimOp_Real_TILDE (* Real.~ *)
                | PrimOp_Real_LT (* Real.< *)
                | PrimOp_Real_LE (* Real.<= *)
                | PrimOp_Real_GT (* Real.> *)
                | PrimOp_Real_GE (* Real.>= *)
                | PrimOp_Char_EQUAL (* Char.= *)
                | PrimOp_Char_LT (* Char.< *)
                | PrimOp_Char_LE (* Char.<= *)
                | PrimOp_Char_GT (* Char.> *)
                | PrimOp_Char_GE (* Char.>= *)
                | PrimOp_WideChar_EQUAL (* WideChar.= *)
                | PrimOp_WideChar_LT (* WideChar.< *)
                | PrimOp_WideChar_LE (* WideChar.<= *)
                | PrimOp_WideChar_GT (* WideChar.> *)
                | PrimOp_WideChar_GE (* WideChar.>= *)
                | PrimOp_String_EQUAL (* String.= *)
                | PrimOp_String_LT (* String.< *)
                | PrimOp_String_LE (* String.<= *)
                | PrimOp_String_GT (* String.> *)
                | PrimOp_String_GE (* String.>= *)
                | PrimOp_String_HAT (* String.^ *)
                | PrimOp_String_size (* String.size *)
                | PrimOp_String_str (* String.str *)
                | PrimOp_WideString_EQUAL (* WideString.= *)
                | PrimOp_WideString_LT (* WideString.< *)
                | PrimOp_WideString_LE (* WideString.<= *)
                | PrimOp_WideString_GT (* WideString.> *)
                | PrimOp_WideString_GE (* WideString.>= *)
                | PrimOp_WideString_HAT (* WideString.^ *)
                | PrimOp_WideString_size (* WideString.size *)
                | PrimOp_WideString_str (* WideString.str *)
                | PrimOp_IntInf_EQUAL (* IntInf.= *)
                | PrimOp_IntInf_PLUS (* IntInf.+ *)
                | PrimOp_IntInf_MINUS (* IntInf.- *)
                | PrimOp_IntInf_TIMES (* IntInf.* *)
                | PrimOp_IntInf_TILDE (* IntInf.~ *)
                | PrimOp_IntInf_LT (* IntInf.< *)
                | PrimOp_IntInf_LE (* IntInf.<= *)
                | PrimOp_IntInf_GT (* IntInf.> *)
                | PrimOp_IntInf_GE (* IntInf.>= *)
                | PrimOp_IntInf_andb (* IntInf.andb *)
                | PrimOp_IntInf_orb (* IntInf.orb *)
                | PrimOp_IntInf_xorb (* IntInf.xorb *)
                | PrimOp_IntInf_notb (* IntInf.notb *)
                | PrimOp_IntInf_quot_unchecked (* IntInf.quot.unchecked *)
                | PrimOp_IntInf_rem_unchecked (* IntInf.rem.unchecked *)
                | PrimOp_Vector_EQUAL (* Vector.= *)
                | PrimOp_Vector_length (* Vector.length *)
                | PrimOp_Array_EQUAL (* Array.= *)
                | PrimOp_Array_length (* Array.length *)
                | PrimOp_Unsafe_cast (* Unsafe.cast *)
                | PrimOp_Unsafe_Vector_sub (* Unsafe.Vector.sub *)
                | PrimOp_Unsafe_Array_sub (* Unsafe.Array.sub *)
                | PrimOp_Unsafe_Array_update (* Unsafe.Array.update *)
                | PrimOp_Lua_sub (* Lua.sub *)
                | PrimOp_Lua_set (* Lua.set *)
                | PrimOp_Lua_isNil (* Lua.isNil *)
                | PrimOp_Lua_EQUAL (* Lua.== *)
                | PrimOp_Lua_NOTEQUAL (* Lua.~= *)
                | PrimOp_Lua_LT (* Lua.< *)
                | PrimOp_Lua_LE (* Lua.<= *)
                | PrimOp_Lua_GT (* Lua.> *)
                | PrimOp_Lua_GE (* Lua.>= *)
                | PrimOp_Lua_PLUS (* Lua.+ *)
                | PrimOp_Lua_MINUS (* Lua.- *)
                | PrimOp_Lua_TIMES (* Lua.* *)
                | PrimOp_Lua_DIVIDE (* Lua./ *)
                | PrimOp_Lua_INTDIV (* Lua.// *)
                | PrimOp_Lua_MOD (* Lua.% *)
                | PrimOp_Lua_pow (* Lua.pow *)
                | PrimOp_Lua_unm (* Lua.unm *)
                | PrimOp_Lua_andb (* Lua.andb *)
                | PrimOp_Lua_orb (* Lua.orb *)
                | PrimOp_Lua_xorb (* Lua.xorb *)
                | PrimOp_Lua_notb (* Lua.notb *)
                | PrimOp_Lua_LSHIFT (* Lua.<< *)
                | PrimOp_Lua_RSHIFT (* Lua.>> *)
                | PrimOp_Lua_concat (* Lua.concat *)
                | PrimOp_Lua_length (* Lua.length *)
                | PrimOp_Lua_isFalsy (* Lua.isFalsy *)
                | PrimOp_JavaScript_sub (* JavaScript.sub *)
                | PrimOp_JavaScript_set (* JavaScript.set *)
                | PrimOp_JavaScript_EQUAL (* JavaScript.=== *)
                | PrimOp_JavaScript_NOTEQUAL (* JavaScript.!== *)
                | PrimOp_JavaScript_LT (* JavaScript.< *)
                | PrimOp_JavaScript_LE (* JavaScript.<= *)
                | PrimOp_JavaScript_GT (* JavaScript.> *)
                | PrimOp_JavaScript_GE (* JavaScript.>= *)
                | PrimOp_JavaScript_PLUS (* JavaScript.+ *)
                | PrimOp_JavaScript_MINUS (* JavaScript.- *)
                | PrimOp_JavaScript_TIMES (* JavaScript.* *)
                | PrimOp_JavaScript_DIVIDE (* JavaScript./ *)
                | PrimOp_JavaScript_MOD (* JavaScript.% *)
                | PrimOp_JavaScript_negate (* JavaScript.negate *)
                | PrimOp_JavaScript_andb (* JavaScript.andb *)
                | PrimOp_JavaScript_orb (* JavaScript.orb *)
                | PrimOp_JavaScript_xorb (* JavaScript.xorb *)
                | PrimOp_JavaScript_notb (* JavaScript.notb *)
                | PrimOp_JavaScript_LSHIFT (* JavaScript.<< *)
                | PrimOp_JavaScript_RSHIFT (* JavaScript.>> *)
                | PrimOp_JavaScript_URSHIFT (* JavaScript.>>> *)
                | PrimOp_JavaScript_EXP (* JavaScript.** *)
                | PrimOp_JavaScript_isFalsy (* JavaScript.isFalsy *)
                | PrimOp_JavaScript_typeof (* JavaScript.typeof *)
                | PrimOp_JavaScript_global (* JavaScript.global *)
fun toString PrimOp_EQUAL = "="
  | toString PrimOp_call2 = "call2"
  | toString PrimOp_call3 = "call3"
  | toString PrimOp_Ref_EQUAL = "Ref.="
  | toString PrimOp_Ref_set = "Ref.:="
  | toString PrimOp_Ref_read = "Ref.!"
  | toString PrimOp_Bool_EQUAL = "Bool.="
  | toString PrimOp_Bool_not = "Bool.not"
  | toString PrimOp_Int_EQUAL = "Int.="
  | toString PrimOp_Int_LT = "Int.<"
  | toString PrimOp_Int_LE = "Int.<="
  | toString PrimOp_Int_GT = "Int.>"
  | toString PrimOp_Int_GE = "Int.>="
  | toString PrimOp_Word_EQUAL = "Word.="
  | toString PrimOp_Word_PLUS = "Word.+"
  | toString PrimOp_Word_MINUS = "Word.-"
  | toString PrimOp_Word_TIMES = "Word.*"
  | toString PrimOp_Word_TILDE = "Word.~"
  | toString PrimOp_Word_LT = "Word.<"
  | toString PrimOp_Word_LE = "Word.<="
  | toString PrimOp_Word_GT = "Word.>"
  | toString PrimOp_Word_GE = "Word.>="
  | toString PrimOp_Real_PLUS = "Real.+"
  | toString PrimOp_Real_MINUS = "Real.-"
  | toString PrimOp_Real_TIMES = "Real.*"
  | toString PrimOp_Real_DIVIDE = "Real./"
  | toString PrimOp_Real_TILDE = "Real.~"
  | toString PrimOp_Real_LT = "Real.<"
  | toString PrimOp_Real_LE = "Real.<="
  | toString PrimOp_Real_GT = "Real.>"
  | toString PrimOp_Real_GE = "Real.>="
  | toString PrimOp_Char_EQUAL = "Char.="
  | toString PrimOp_Char_LT = "Char.<"
  | toString PrimOp_Char_LE = "Char.<="
  | toString PrimOp_Char_GT = "Char.>"
  | toString PrimOp_Char_GE = "Char.>="
  | toString PrimOp_WideChar_EQUAL = "WideChar.="
  | toString PrimOp_WideChar_LT = "WideChar.<"
  | toString PrimOp_WideChar_LE = "WideChar.<="
  | toString PrimOp_WideChar_GT = "WideChar.>"
  | toString PrimOp_WideChar_GE = "WideChar.>="
  | toString PrimOp_String_EQUAL = "String.="
  | toString PrimOp_String_LT = "String.<"
  | toString PrimOp_String_LE = "String.<="
  | toString PrimOp_String_GT = "String.>"
  | toString PrimOp_String_GE = "String.>="
  | toString PrimOp_String_HAT = "String.^"
  | toString PrimOp_String_size = "String.size"
  | toString PrimOp_String_str = "String.str"
  | toString PrimOp_WideString_EQUAL = "WideString.="
  | toString PrimOp_WideString_LT = "WideString.<"
  | toString PrimOp_WideString_LE = "WideString.<="
  | toString PrimOp_WideString_GT = "WideString.>"
  | toString PrimOp_WideString_GE = "WideString.>="
  | toString PrimOp_WideString_HAT = "WideString.^"
  | toString PrimOp_WideString_size = "WideString.size"
  | toString PrimOp_WideString_str = "WideString.str"
  | toString PrimOp_IntInf_EQUAL = "IntInf.="
  | toString PrimOp_IntInf_PLUS = "IntInf.+"
  | toString PrimOp_IntInf_MINUS = "IntInf.-"
  | toString PrimOp_IntInf_TIMES = "IntInf.*"
  | toString PrimOp_IntInf_TILDE = "IntInf.~"
  | toString PrimOp_IntInf_LT = "IntInf.<"
  | toString PrimOp_IntInf_LE = "IntInf.<="
  | toString PrimOp_IntInf_GT = "IntInf.>"
  | toString PrimOp_IntInf_GE = "IntInf.>="
  | toString PrimOp_IntInf_andb = "IntInf.andb"
  | toString PrimOp_IntInf_orb = "IntInf.orb"
  | toString PrimOp_IntInf_xorb = "IntInf.xorb"
  | toString PrimOp_IntInf_notb = "IntInf.notb"
  | toString PrimOp_IntInf_quot_unchecked = "IntInf.quot.unchecked"
  | toString PrimOp_IntInf_rem_unchecked = "IntInf.rem.unchecked"
  | toString PrimOp_Vector_EQUAL = "Vector.="
  | toString PrimOp_Vector_length = "Vector.length"
  | toString PrimOp_Array_EQUAL = "Array.="
  | toString PrimOp_Array_length = "Array.length"
  | toString PrimOp_Unsafe_cast = "Unsafe.cast"
  | toString PrimOp_Unsafe_Vector_sub = "Unsafe.Vector.sub"
  | toString PrimOp_Unsafe_Array_sub = "Unsafe.Array.sub"
  | toString PrimOp_Unsafe_Array_update = "Unsafe.Array.update"
  | toString PrimOp_Lua_sub = "Lua.sub"
  | toString PrimOp_Lua_set = "Lua.set"
  | toString PrimOp_Lua_isNil = "Lua.isNil"
  | toString PrimOp_Lua_EQUAL = "Lua.=="
  | toString PrimOp_Lua_NOTEQUAL = "Lua.~="
  | toString PrimOp_Lua_LT = "Lua.<"
  | toString PrimOp_Lua_LE = "Lua.<="
  | toString PrimOp_Lua_GT = "Lua.>"
  | toString PrimOp_Lua_GE = "Lua.>="
  | toString PrimOp_Lua_PLUS = "Lua.+"
  | toString PrimOp_Lua_MINUS = "Lua.-"
  | toString PrimOp_Lua_TIMES = "Lua.*"
  | toString PrimOp_Lua_DIVIDE = "Lua./"
  | toString PrimOp_Lua_INTDIV = "Lua.//"
  | toString PrimOp_Lua_MOD = "Lua.%"
  | toString PrimOp_Lua_pow = "Lua.pow"
  | toString PrimOp_Lua_unm = "Lua.unm"
  | toString PrimOp_Lua_andb = "Lua.andb"
  | toString PrimOp_Lua_orb = "Lua.orb"
  | toString PrimOp_Lua_xorb = "Lua.xorb"
  | toString PrimOp_Lua_notb = "Lua.notb"
  | toString PrimOp_Lua_LSHIFT = "Lua.<<"
  | toString PrimOp_Lua_RSHIFT = "Lua.>>"
  | toString PrimOp_Lua_concat = "Lua.concat"
  | toString PrimOp_Lua_length = "Lua.length"
  | toString PrimOp_Lua_isFalsy = "Lua.isFalsy"
  | toString PrimOp_JavaScript_sub = "JavaScript.sub"
  | toString PrimOp_JavaScript_set = "JavaScript.set"
  | toString PrimOp_JavaScript_EQUAL = "JavaScript.==="
  | toString PrimOp_JavaScript_NOTEQUAL = "JavaScript.!=="
  | toString PrimOp_JavaScript_LT = "JavaScript.<"
  | toString PrimOp_JavaScript_LE = "JavaScript.<="
  | toString PrimOp_JavaScript_GT = "JavaScript.>"
  | toString PrimOp_JavaScript_GE = "JavaScript.>="
  | toString PrimOp_JavaScript_PLUS = "JavaScript.+"
  | toString PrimOp_JavaScript_MINUS = "JavaScript.-"
  | toString PrimOp_JavaScript_TIMES = "JavaScript.*"
  | toString PrimOp_JavaScript_DIVIDE = "JavaScript./"
  | toString PrimOp_JavaScript_MOD = "JavaScript.%"
  | toString PrimOp_JavaScript_negate = "JavaScript.negate"
  | toString PrimOp_JavaScript_andb = "JavaScript.andb"
  | toString PrimOp_JavaScript_orb = "JavaScript.orb"
  | toString PrimOp_JavaScript_xorb = "JavaScript.xorb"
  | toString PrimOp_JavaScript_notb = "JavaScript.notb"
  | toString PrimOp_JavaScript_LSHIFT = "JavaScript.<<"
  | toString PrimOp_JavaScript_RSHIFT = "JavaScript.>>"
  | toString PrimOp_JavaScript_URSHIFT = "JavaScript.>>>"
  | toString PrimOp_JavaScript_EXP = "JavaScript.**"
  | toString PrimOp_JavaScript_isFalsy = "JavaScript.isFalsy"
  | toString PrimOp_JavaScript_typeof = "JavaScript.typeof"
  | toString PrimOp_JavaScript_global = "JavaScript.global"
fun fromString "=" = SOME PrimOp_EQUAL
  | fromString "call2" = SOME PrimOp_call2
  | fromString "call3" = SOME PrimOp_call3
  | fromString "Ref.=" = SOME PrimOp_Ref_EQUAL
  | fromString "Ref.:=" = SOME PrimOp_Ref_set
  | fromString "Ref.!" = SOME PrimOp_Ref_read
  | fromString "Bool.=" = SOME PrimOp_Bool_EQUAL
  | fromString "Bool.not" = SOME PrimOp_Bool_not
  | fromString "Int.=" = SOME PrimOp_Int_EQUAL
  | fromString "Int.<" = SOME PrimOp_Int_LT
  | fromString "Int.<=" = SOME PrimOp_Int_LE
  | fromString "Int.>" = SOME PrimOp_Int_GT
  | fromString "Int.>=" = SOME PrimOp_Int_GE
  | fromString "Word.=" = SOME PrimOp_Word_EQUAL
  | fromString "Word.+" = SOME PrimOp_Word_PLUS
  | fromString "Word.-" = SOME PrimOp_Word_MINUS
  | fromString "Word.*" = SOME PrimOp_Word_TIMES
  | fromString "Word.~" = SOME PrimOp_Word_TILDE
  | fromString "Word.<" = SOME PrimOp_Word_LT
  | fromString "Word.<=" = SOME PrimOp_Word_LE
  | fromString "Word.>" = SOME PrimOp_Word_GT
  | fromString "Word.>=" = SOME PrimOp_Word_GE
  | fromString "Real.+" = SOME PrimOp_Real_PLUS
  | fromString "Real.-" = SOME PrimOp_Real_MINUS
  | fromString "Real.*" = SOME PrimOp_Real_TIMES
  | fromString "Real./" = SOME PrimOp_Real_DIVIDE
  | fromString "Real.~" = SOME PrimOp_Real_TILDE
  | fromString "Real.<" = SOME PrimOp_Real_LT
  | fromString "Real.<=" = SOME PrimOp_Real_LE
  | fromString "Real.>" = SOME PrimOp_Real_GT
  | fromString "Real.>=" = SOME PrimOp_Real_GE
  | fromString "Char.=" = SOME PrimOp_Char_EQUAL
  | fromString "Char.<" = SOME PrimOp_Char_LT
  | fromString "Char.<=" = SOME PrimOp_Char_LE
  | fromString "Char.>" = SOME PrimOp_Char_GT
  | fromString "Char.>=" = SOME PrimOp_Char_GE
  | fromString "WideChar.=" = SOME PrimOp_WideChar_EQUAL
  | fromString "WideChar.<" = SOME PrimOp_WideChar_LT
  | fromString "WideChar.<=" = SOME PrimOp_WideChar_LE
  | fromString "WideChar.>" = SOME PrimOp_WideChar_GT
  | fromString "WideChar.>=" = SOME PrimOp_WideChar_GE
  | fromString "String.=" = SOME PrimOp_String_EQUAL
  | fromString "String.<" = SOME PrimOp_String_LT
  | fromString "String.<=" = SOME PrimOp_String_LE
  | fromString "String.>" = SOME PrimOp_String_GT
  | fromString "String.>=" = SOME PrimOp_String_GE
  | fromString "String.^" = SOME PrimOp_String_HAT
  | fromString "String.size" = SOME PrimOp_String_size
  | fromString "String.str" = SOME PrimOp_String_str
  | fromString "WideString.=" = SOME PrimOp_WideString_EQUAL
  | fromString "WideString.<" = SOME PrimOp_WideString_LT
  | fromString "WideString.<=" = SOME PrimOp_WideString_LE
  | fromString "WideString.>" = SOME PrimOp_WideString_GT
  | fromString "WideString.>=" = SOME PrimOp_WideString_GE
  | fromString "WideString.^" = SOME PrimOp_WideString_HAT
  | fromString "WideString.size" = SOME PrimOp_WideString_size
  | fromString "WideString.str" = SOME PrimOp_WideString_str
  | fromString "IntInf.=" = SOME PrimOp_IntInf_EQUAL
  | fromString "IntInf.+" = SOME PrimOp_IntInf_PLUS
  | fromString "IntInf.-" = SOME PrimOp_IntInf_MINUS
  | fromString "IntInf.*" = SOME PrimOp_IntInf_TIMES
  | fromString "IntInf.~" = SOME PrimOp_IntInf_TILDE
  | fromString "IntInf.<" = SOME PrimOp_IntInf_LT
  | fromString "IntInf.<=" = SOME PrimOp_IntInf_LE
  | fromString "IntInf.>" = SOME PrimOp_IntInf_GT
  | fromString "IntInf.>=" = SOME PrimOp_IntInf_GE
  | fromString "IntInf.andb" = SOME PrimOp_IntInf_andb
  | fromString "IntInf.orb" = SOME PrimOp_IntInf_orb
  | fromString "IntInf.xorb" = SOME PrimOp_IntInf_xorb
  | fromString "IntInf.notb" = SOME PrimOp_IntInf_notb
  | fromString "IntInf.quot.unchecked" = SOME PrimOp_IntInf_quot_unchecked
  | fromString "IntInf.rem.unchecked" = SOME PrimOp_IntInf_rem_unchecked
  | fromString "Vector.=" = SOME PrimOp_Vector_EQUAL
  | fromString "Vector.length" = SOME PrimOp_Vector_length
  | fromString "Array.=" = SOME PrimOp_Array_EQUAL
  | fromString "Array.length" = SOME PrimOp_Array_length
  | fromString "Unsafe.cast" = SOME PrimOp_Unsafe_cast
  | fromString "Unsafe.Vector.sub" = SOME PrimOp_Unsafe_Vector_sub
  | fromString "Unsafe.Array.sub" = SOME PrimOp_Unsafe_Array_sub
  | fromString "Unsafe.Array.update" = SOME PrimOp_Unsafe_Array_update
  | fromString "Lua.sub" = SOME PrimOp_Lua_sub
  | fromString "Lua.set" = SOME PrimOp_Lua_set
  | fromString "Lua.isNil" = SOME PrimOp_Lua_isNil
  | fromString "Lua.==" = SOME PrimOp_Lua_EQUAL
  | fromString "Lua.~=" = SOME PrimOp_Lua_NOTEQUAL
  | fromString "Lua.<" = SOME PrimOp_Lua_LT
  | fromString "Lua.<=" = SOME PrimOp_Lua_LE
  | fromString "Lua.>" = SOME PrimOp_Lua_GT
  | fromString "Lua.>=" = SOME PrimOp_Lua_GE
  | fromString "Lua.+" = SOME PrimOp_Lua_PLUS
  | fromString "Lua.-" = SOME PrimOp_Lua_MINUS
  | fromString "Lua.*" = SOME PrimOp_Lua_TIMES
  | fromString "Lua./" = SOME PrimOp_Lua_DIVIDE
  | fromString "Lua.//" = SOME PrimOp_Lua_INTDIV
  | fromString "Lua.%" = SOME PrimOp_Lua_MOD
  | fromString "Lua.pow" = SOME PrimOp_Lua_pow
  | fromString "Lua.unm" = SOME PrimOp_Lua_unm
  | fromString "Lua.andb" = SOME PrimOp_Lua_andb
  | fromString "Lua.orb" = SOME PrimOp_Lua_orb
  | fromString "Lua.xorb" = SOME PrimOp_Lua_xorb
  | fromString "Lua.notb" = SOME PrimOp_Lua_notb
  | fromString "Lua.<<" = SOME PrimOp_Lua_LSHIFT
  | fromString "Lua.>>" = SOME PrimOp_Lua_RSHIFT
  | fromString "Lua.concat" = SOME PrimOp_Lua_concat
  | fromString "Lua.length" = SOME PrimOp_Lua_length
  | fromString "Lua.isFalsy" = SOME PrimOp_Lua_isFalsy
  | fromString "JavaScript.sub" = SOME PrimOp_JavaScript_sub
  | fromString "JavaScript.set" = SOME PrimOp_JavaScript_set
  | fromString "JavaScript.===" = SOME PrimOp_JavaScript_EQUAL
  | fromString "JavaScript.!==" = SOME PrimOp_JavaScript_NOTEQUAL
  | fromString "JavaScript.<" = SOME PrimOp_JavaScript_LT
  | fromString "JavaScript.<=" = SOME PrimOp_JavaScript_LE
  | fromString "JavaScript.>" = SOME PrimOp_JavaScript_GT
  | fromString "JavaScript.>=" = SOME PrimOp_JavaScript_GE
  | fromString "JavaScript.+" = SOME PrimOp_JavaScript_PLUS
  | fromString "JavaScript.-" = SOME PrimOp_JavaScript_MINUS
  | fromString "JavaScript.*" = SOME PrimOp_JavaScript_TIMES
  | fromString "JavaScript./" = SOME PrimOp_JavaScript_DIVIDE
  | fromString "JavaScript.%" = SOME PrimOp_JavaScript_MOD
  | fromString "JavaScript.negate" = SOME PrimOp_JavaScript_negate
  | fromString "JavaScript.andb" = SOME PrimOp_JavaScript_andb
  | fromString "JavaScript.orb" = SOME PrimOp_JavaScript_orb
  | fromString "JavaScript.xorb" = SOME PrimOp_JavaScript_xorb
  | fromString "JavaScript.notb" = SOME PrimOp_JavaScript_notb
  | fromString "JavaScript.<<" = SOME PrimOp_JavaScript_LSHIFT
  | fromString "JavaScript.>>" = SOME PrimOp_JavaScript_RSHIFT
  | fromString "JavaScript.>>>" = SOME PrimOp_JavaScript_URSHIFT
  | fromString "JavaScript.**" = SOME PrimOp_JavaScript_EXP
  | fromString "JavaScript.isFalsy" = SOME PrimOp_JavaScript_isFalsy
  | fromString "JavaScript.typeof" = SOME PrimOp_JavaScript_typeof
  | fromString "JavaScript.global" = SOME PrimOp_JavaScript_global
  | fromString _ = NONE
end;

functor TypeOfPrimitives (type ty
                          type tv
                          type constraint
                          val tyVarA : tv
                          val tyVarB : tv
                          val tyVarC : tv
                          val tyVarD : tv
                          val tyVarEqA : tv
                          val tyA : ty
                          val tyB : ty
                          val tyC : ty
                          val tyD : ty
                          val tyEqA : ty
                          val unit : ty
                          val bool : ty
                          val int : ty
                          val word : ty
                          val real : ty
                          val char : ty
                          val wideChar : ty
                          val string : ty
                          val wideString : ty
                          val intInf : ty
                          val LuaValue : ty
                          val JavaScriptValue : ty
                          val refOf : ty -> ty
                          val vectorOf : ty -> ty
                          val arrayOf : ty -> ty
                          val pairOf : ty * ty -> ty
                          val function1Of : ty * ty -> ty
                          val function2Of : ty * ty * ty -> ty
                          val function3Of : ty * ty * ty * ty -> ty
                          val IsEqType : constraint
                         ) : sig
                               val typeOf : Primitives.PrimOp -> { vars : (tv * constraint list) list, args : ty vector, result : ty }
                             end = struct
fun typeOf Primitives.PrimOp_EQUAL = { vars = [(tyVarEqA, [IsEqType])], args = vector [tyEqA, tyEqA], result = bool }
  | typeOf Primitives.PrimOp_call2 = { vars = [(tyVarA, []), (tyVarB, []), (tyVarC, [])], args = vector [function2Of (tyA, tyB, tyC), tyB, tyC], result = tyA }
  | typeOf Primitives.PrimOp_call3 = { vars = [(tyVarA, []), (tyVarB, []), (tyVarC, []), (tyVarD, [])], args = vector [function3Of (tyA, tyB, tyC, tyD), tyB, tyC, tyD], result = tyA }
  | typeOf Primitives.PrimOp_Ref_EQUAL = { vars = [(tyVarA, [])], args = vector [refOf (tyA), refOf (tyA)], result = bool }
  | typeOf Primitives.PrimOp_Ref_set = { vars = [(tyVarA, [])], args = vector [refOf (tyA), tyA], result = unit }
  | typeOf Primitives.PrimOp_Ref_read = { vars = [(tyVarA, [])], args = vector [refOf (tyA)], result = tyA }
  | typeOf Primitives.PrimOp_Bool_EQUAL = { vars = [], args = vector [bool, bool], result = bool }
  | typeOf Primitives.PrimOp_Bool_not = { vars = [], args = vector [bool], result = bool }
  | typeOf Primitives.PrimOp_Int_EQUAL = { vars = [], args = vector [int, int], result = bool }
  | typeOf Primitives.PrimOp_Int_LT = { vars = [], args = vector [int, int], result = bool }
  | typeOf Primitives.PrimOp_Int_LE = { vars = [], args = vector [int, int], result = bool }
  | typeOf Primitives.PrimOp_Int_GT = { vars = [], args = vector [int, int], result = bool }
  | typeOf Primitives.PrimOp_Int_GE = { vars = [], args = vector [int, int], result = bool }
  | typeOf Primitives.PrimOp_Word_EQUAL = { vars = [], args = vector [word, word], result = bool }
  | typeOf Primitives.PrimOp_Word_PLUS = { vars = [], args = vector [word, word], result = word }
  | typeOf Primitives.PrimOp_Word_MINUS = { vars = [], args = vector [word, word], result = word }
  | typeOf Primitives.PrimOp_Word_TIMES = { vars = [], args = vector [word, word], result = word }
  | typeOf Primitives.PrimOp_Word_TILDE = { vars = [], args = vector [word], result = word }
  | typeOf Primitives.PrimOp_Word_LT = { vars = [], args = vector [word, word], result = bool }
  | typeOf Primitives.PrimOp_Word_LE = { vars = [], args = vector [word, word], result = bool }
  | typeOf Primitives.PrimOp_Word_GT = { vars = [], args = vector [word, word], result = bool }
  | typeOf Primitives.PrimOp_Word_GE = { vars = [], args = vector [word, word], result = bool }
  | typeOf Primitives.PrimOp_Real_PLUS = { vars = [], args = vector [real, real], result = real }
  | typeOf Primitives.PrimOp_Real_MINUS = { vars = [], args = vector [real, real], result = real }
  | typeOf Primitives.PrimOp_Real_TIMES = { vars = [], args = vector [real, real], result = real }
  | typeOf Primitives.PrimOp_Real_DIVIDE = { vars = [], args = vector [real, real], result = real }
  | typeOf Primitives.PrimOp_Real_TILDE = { vars = [], args = vector [real], result = real }
  | typeOf Primitives.PrimOp_Real_LT = { vars = [], args = vector [real, real], result = bool }
  | typeOf Primitives.PrimOp_Real_LE = { vars = [], args = vector [real, real], result = bool }
  | typeOf Primitives.PrimOp_Real_GT = { vars = [], args = vector [real, real], result = bool }
  | typeOf Primitives.PrimOp_Real_GE = { vars = [], args = vector [real, real], result = bool }
  | typeOf Primitives.PrimOp_Char_EQUAL = { vars = [], args = vector [char, char], result = bool }
  | typeOf Primitives.PrimOp_Char_LT = { vars = [], args = vector [char, char], result = bool }
  | typeOf Primitives.PrimOp_Char_LE = { vars = [], args = vector [char, char], result = bool }
  | typeOf Primitives.PrimOp_Char_GT = { vars = [], args = vector [char, char], result = bool }
  | typeOf Primitives.PrimOp_Char_GE = { vars = [], args = vector [char, char], result = bool }
  | typeOf Primitives.PrimOp_WideChar_EQUAL = { vars = [], args = vector [wideChar, wideChar], result = bool }
  | typeOf Primitives.PrimOp_WideChar_LT = { vars = [], args = vector [wideChar, wideChar], result = bool }
  | typeOf Primitives.PrimOp_WideChar_LE = { vars = [], args = vector [wideChar, wideChar], result = bool }
  | typeOf Primitives.PrimOp_WideChar_GT = { vars = [], args = vector [wideChar, wideChar], result = bool }
  | typeOf Primitives.PrimOp_WideChar_GE = { vars = [], args = vector [wideChar, wideChar], result = bool }
  | typeOf Primitives.PrimOp_String_EQUAL = { vars = [], args = vector [string, string], result = bool }
  | typeOf Primitives.PrimOp_String_LT = { vars = [], args = vector [string, string], result = bool }
  | typeOf Primitives.PrimOp_String_LE = { vars = [], args = vector [string, string], result = bool }
  | typeOf Primitives.PrimOp_String_GT = { vars = [], args = vector [string, string], result = bool }
  | typeOf Primitives.PrimOp_String_GE = { vars = [], args = vector [string, string], result = bool }
  | typeOf Primitives.PrimOp_String_HAT = { vars = [], args = vector [string, string], result = string }
  | typeOf Primitives.PrimOp_String_size = { vars = [], args = vector [string], result = int }
  | typeOf Primitives.PrimOp_String_str = { vars = [], args = vector [char], result = string }
  | typeOf Primitives.PrimOp_WideString_EQUAL = { vars = [], args = vector [wideString, wideString], result = bool }
  | typeOf Primitives.PrimOp_WideString_LT = { vars = [], args = vector [wideString, wideString], result = bool }
  | typeOf Primitives.PrimOp_WideString_LE = { vars = [], args = vector [wideString, wideString], result = bool }
  | typeOf Primitives.PrimOp_WideString_GT = { vars = [], args = vector [wideString, wideString], result = bool }
  | typeOf Primitives.PrimOp_WideString_GE = { vars = [], args = vector [wideString, wideString], result = bool }
  | typeOf Primitives.PrimOp_WideString_HAT = { vars = [], args = vector [wideString, wideString], result = wideString }
  | typeOf Primitives.PrimOp_WideString_size = { vars = [], args = vector [wideString], result = int }
  | typeOf Primitives.PrimOp_WideString_str = { vars = [], args = vector [wideChar], result = wideString }
  | typeOf Primitives.PrimOp_IntInf_EQUAL = { vars = [], args = vector [intInf, intInf], result = bool }
  | typeOf Primitives.PrimOp_IntInf_PLUS = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.PrimOp_IntInf_MINUS = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.PrimOp_IntInf_TIMES = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.PrimOp_IntInf_TILDE = { vars = [], args = vector [intInf], result = intInf }
  | typeOf Primitives.PrimOp_IntInf_LT = { vars = [], args = vector [intInf, intInf], result = bool }
  | typeOf Primitives.PrimOp_IntInf_LE = { vars = [], args = vector [intInf, intInf], result = bool }
  | typeOf Primitives.PrimOp_IntInf_GT = { vars = [], args = vector [intInf, intInf], result = bool }
  | typeOf Primitives.PrimOp_IntInf_GE = { vars = [], args = vector [intInf, intInf], result = bool }
  | typeOf Primitives.PrimOp_IntInf_andb = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.PrimOp_IntInf_orb = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.PrimOp_IntInf_xorb = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.PrimOp_IntInf_notb = { vars = [], args = vector [intInf], result = intInf }
  | typeOf Primitives.PrimOp_IntInf_quot_unchecked = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.PrimOp_IntInf_rem_unchecked = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.PrimOp_Vector_EQUAL = { vars = [(tyVarA, [])], args = vector [function1Of (bool, pairOf (tyA, tyA)), vectorOf (tyA), vectorOf (tyA)], result = bool }
  | typeOf Primitives.PrimOp_Vector_length = { vars = [(tyVarA, [])], args = vector [vectorOf (tyA)], result = int }
  | typeOf Primitives.PrimOp_Array_EQUAL = { vars = [(tyVarA, [])], args = vector [arrayOf (tyA), arrayOf (tyA)], result = bool }
  | typeOf Primitives.PrimOp_Array_length = { vars = [(tyVarA, [])], args = vector [arrayOf (tyA)], result = int }
  | typeOf Primitives.PrimOp_Unsafe_cast = { vars = [(tyVarA, []), (tyVarB, [])], args = vector [tyA], result = tyB }
  | typeOf Primitives.PrimOp_Unsafe_Vector_sub = { vars = [(tyVarA, [])], args = vector [vectorOf (tyA), int], result = tyA }
  | typeOf Primitives.PrimOp_Unsafe_Array_sub = { vars = [(tyVarA, [])], args = vector [arrayOf (tyA), int], result = tyA }
  | typeOf Primitives.PrimOp_Unsafe_Array_update = { vars = [(tyVarA, [])], args = vector [arrayOf (tyA), int, tyA], result = unit }
  | typeOf Primitives.PrimOp_Lua_sub = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_set = { vars = [], args = vector [LuaValue, LuaValue, LuaValue], result = unit }
  | typeOf Primitives.PrimOp_Lua_isNil = { vars = [], args = vector [LuaValue], result = bool }
  | typeOf Primitives.PrimOp_Lua_EQUAL = { vars = [], args = vector [LuaValue, LuaValue], result = bool }
  | typeOf Primitives.PrimOp_Lua_NOTEQUAL = { vars = [], args = vector [LuaValue, LuaValue], result = bool }
  | typeOf Primitives.PrimOp_Lua_LT = { vars = [], args = vector [LuaValue, LuaValue], result = bool }
  | typeOf Primitives.PrimOp_Lua_LE = { vars = [], args = vector [LuaValue, LuaValue], result = bool }
  | typeOf Primitives.PrimOp_Lua_GT = { vars = [], args = vector [LuaValue, LuaValue], result = bool }
  | typeOf Primitives.PrimOp_Lua_GE = { vars = [], args = vector [LuaValue, LuaValue], result = bool }
  | typeOf Primitives.PrimOp_Lua_PLUS = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_MINUS = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_TIMES = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_DIVIDE = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_INTDIV = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_MOD = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_pow = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_unm = { vars = [], args = vector [LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_andb = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_orb = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_xorb = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_notb = { vars = [], args = vector [LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_LSHIFT = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_RSHIFT = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_concat = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_length = { vars = [], args = vector [LuaValue], result = LuaValue }
  | typeOf Primitives.PrimOp_Lua_isFalsy = { vars = [], args = vector [LuaValue], result = bool }
  | typeOf Primitives.PrimOp_JavaScript_sub = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_set = { vars = [], args = vector [JavaScriptValue, JavaScriptValue, JavaScriptValue], result = unit }
  | typeOf Primitives.PrimOp_JavaScript_EQUAL = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = bool }
  | typeOf Primitives.PrimOp_JavaScript_NOTEQUAL = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = bool }
  | typeOf Primitives.PrimOp_JavaScript_LT = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = bool }
  | typeOf Primitives.PrimOp_JavaScript_LE = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = bool }
  | typeOf Primitives.PrimOp_JavaScript_GT = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = bool }
  | typeOf Primitives.PrimOp_JavaScript_GE = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = bool }
  | typeOf Primitives.PrimOp_JavaScript_PLUS = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_MINUS = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_TIMES = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_DIVIDE = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_MOD = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_negate = { vars = [], args = vector [JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_andb = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_orb = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_xorb = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_notb = { vars = [], args = vector [JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_LSHIFT = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_RSHIFT = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_URSHIFT = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_EXP = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.PrimOp_JavaScript_isFalsy = { vars = [], args = vector [JavaScriptValue], result = bool }
  | typeOf Primitives.PrimOp_JavaScript_typeof = { vars = [], args = vector [JavaScriptValue], result = wideString }
  | typeOf Primitives.PrimOp_JavaScript_global = { vars = [], args = vector [wideString], result = JavaScriptValue }
end;
