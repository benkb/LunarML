(* This file was generated by primitives.lua *)
structure Primitives = struct
datatype PrimOp = EQUAL (* = *)
                | call2 (* call2 *)
                | call3 (* call3 *)
                | Ref_EQUAL (* Ref.= *)
                | Ref_set (* Ref.:= *)
                | Ref_read (* Ref.! *)
                | Bool_EQUAL (* Bool.= *)
                | Bool_not (* Bool.not *)
                | Int_EQUAL (* Int.= *)
                | Int_LT (* Int.< *)
                | Int_LE (* Int.<= *)
                | Int_GT (* Int.> *)
                | Int_GE (* Int.>= *)
                | Word_EQUAL (* Word.= *)
                | Word_PLUS (* Word.+ *)
                | Word_MINUS (* Word.- *)
                | Word_TIMES (* Word.* *)
                | Word_TILDE (* Word.~ *)
                | Word_LT (* Word.< *)
                | Word_LE (* Word.<= *)
                | Word_GT (* Word.> *)
                | Word_GE (* Word.>= *)
                | Real_PLUS (* Real.+ *)
                | Real_MINUS (* Real.- *)
                | Real_TIMES (* Real.* *)
                | Real_DIVIDE (* Real./ *)
                | Real_TILDE (* Real.~ *)
                | Real_LT (* Real.< *)
                | Real_LE (* Real.<= *)
                | Real_GT (* Real.> *)
                | Real_GE (* Real.>= *)
                | Char_EQUAL (* Char.= *)
                | Char_LT (* Char.< *)
                | Char_LE (* Char.<= *)
                | Char_GT (* Char.> *)
                | Char_GE (* Char.>= *)
                | WideChar_EQUAL (* WideChar.= *)
                | WideChar_LT (* WideChar.< *)
                | WideChar_LE (* WideChar.<= *)
                | WideChar_GT (* WideChar.> *)
                | WideChar_GE (* WideChar.>= *)
                | String_EQUAL (* String.= *)
                | String_LT (* String.< *)
                | String_LE (* String.<= *)
                | String_GT (* String.> *)
                | String_GE (* String.>= *)
                | String_HAT (* String.^ *)
                | String_size (* String.size *)
                | String_str (* String.str *)
                | WideString_EQUAL (* WideString.= *)
                | WideString_LT (* WideString.< *)
                | WideString_LE (* WideString.<= *)
                | WideString_GT (* WideString.> *)
                | WideString_GE (* WideString.>= *)
                | WideString_HAT (* WideString.^ *)
                | WideString_size (* WideString.size *)
                | WideString_str (* WideString.str *)
                | IntInf_EQUAL (* IntInf.= *)
                | IntInf_PLUS (* IntInf.+ *)
                | IntInf_MINUS (* IntInf.- *)
                | IntInf_TIMES (* IntInf.* *)
                | IntInf_TILDE (* IntInf.~ *)
                | IntInf_LT (* IntInf.< *)
                | IntInf_LE (* IntInf.<= *)
                | IntInf_GT (* IntInf.> *)
                | IntInf_GE (* IntInf.>= *)
                | IntInf_andb (* IntInf.andb *)
                | IntInf_orb (* IntInf.orb *)
                | IntInf_xorb (* IntInf.xorb *)
                | IntInf_notb (* IntInf.notb *)
                | IntInf_quot_unchecked (* IntInf.quot.unchecked *)
                | IntInf_rem_unchecked (* IntInf.rem.unchecked *)
                | Vector_length (* Vector.length *)
                | Vector_unsafeFromListRevN (* Vector.unsafeFromListRevN *)
                | Array_EQUAL (* Array.= *)
                | Array_length (* Array.length *)
                | Unsafe_cast (* Unsafe.cast *)
                | Unsafe_Vector_sub (* Unsafe.Vector.sub *)
                | Unsafe_Array_sub (* Unsafe.Array.sub *)
                | Unsafe_Array_update (* Unsafe.Array.update *)
                | Exception_instanceof (* Exception.instanceof *)
                | Cont_callcc (* Cont.callcc *)
                | Cont_throw (* Cont.throw *)
                | DelimCont_newPrompt (* DelimCont.newPrompt *)
                | DelimCont_pushPrompt (* DelimCont.pushPrompt *)
                | DelimCont_withSubCont (* DelimCont.withSubCont *)
                | DelimCont_pushSubCont (* DelimCont.pushSubCont *)
                | Lua_sub (* Lua.sub *)
                | Lua_set (* Lua.set *)
                | Lua_isNil (* Lua.isNil *)
                | Lua_EQUAL (* Lua.== *)
                | Lua_NOTEQUAL (* Lua.~= *)
                | Lua_LT (* Lua.< *)
                | Lua_LE (* Lua.<= *)
                | Lua_GT (* Lua.> *)
                | Lua_GE (* Lua.>= *)
                | Lua_PLUS (* Lua.+ *)
                | Lua_MINUS (* Lua.- *)
                | Lua_TIMES (* Lua.* *)
                | Lua_DIVIDE (* Lua./ *)
                | Lua_INTDIV (* Lua.// *)
                | Lua_MOD (* Lua.% *)
                | Lua_pow (* Lua.pow *)
                | Lua_unm (* Lua.unm *)
                | Lua_andb (* Lua.andb *)
                | Lua_orb (* Lua.orb *)
                | Lua_xorb (* Lua.xorb *)
                | Lua_notb (* Lua.notb *)
                | Lua_LSHIFT (* Lua.<< *)
                | Lua_RSHIFT (* Lua.>> *)
                | Lua_concat (* Lua.concat *)
                | Lua_length (* Lua.length *)
                | Lua_isFalsy (* Lua.isFalsy *)
                | Lua_call0 (* Lua.call0 *)
                | Lua_call1 (* Lua.call1 *)
                | Lua_call2 (* Lua.call2 *)
                | Lua_call3 (* Lua.call3 *)
                | JavaScript_sub (* JavaScript.sub *)
                | JavaScript_set (* JavaScript.set *)
                | JavaScript_EQUAL (* JavaScript.=== *)
                | JavaScript_NOTEQUAL (* JavaScript.!== *)
                | JavaScript_LT (* JavaScript.< *)
                | JavaScript_LE (* JavaScript.<= *)
                | JavaScript_GT (* JavaScript.> *)
                | JavaScript_GE (* JavaScript.>= *)
                | JavaScript_PLUS (* JavaScript.+ *)
                | JavaScript_MINUS (* JavaScript.- *)
                | JavaScript_TIMES (* JavaScript.* *)
                | JavaScript_DIVIDE (* JavaScript./ *)
                | JavaScript_MOD (* JavaScript.% *)
                | JavaScript_negate (* JavaScript.negate *)
                | JavaScript_andb (* JavaScript.andb *)
                | JavaScript_orb (* JavaScript.orb *)
                | JavaScript_xorb (* JavaScript.xorb *)
                | JavaScript_notb (* JavaScript.notb *)
                | JavaScript_LSHIFT (* JavaScript.<< *)
                | JavaScript_RSHIFT (* JavaScript.>> *)
                | JavaScript_URSHIFT (* JavaScript.>>> *)
                | JavaScript_EXP (* JavaScript.** *)
                | JavaScript_isFalsy (* JavaScript.isFalsy *)
                | JavaScript_typeof (* JavaScript.typeof *)
                | JavaScript_global (* JavaScript.global *)
fun toString EQUAL = "="
  | toString call2 = "call2"
  | toString call3 = "call3"
  | toString Ref_EQUAL = "Ref.="
  | toString Ref_set = "Ref.:="
  | toString Ref_read = "Ref.!"
  | toString Bool_EQUAL = "Bool.="
  | toString Bool_not = "Bool.not"
  | toString Int_EQUAL = "Int.="
  | toString Int_LT = "Int.<"
  | toString Int_LE = "Int.<="
  | toString Int_GT = "Int.>"
  | toString Int_GE = "Int.>="
  | toString Word_EQUAL = "Word.="
  | toString Word_PLUS = "Word.+"
  | toString Word_MINUS = "Word.-"
  | toString Word_TIMES = "Word.*"
  | toString Word_TILDE = "Word.~"
  | toString Word_LT = "Word.<"
  | toString Word_LE = "Word.<="
  | toString Word_GT = "Word.>"
  | toString Word_GE = "Word.>="
  | toString Real_PLUS = "Real.+"
  | toString Real_MINUS = "Real.-"
  | toString Real_TIMES = "Real.*"
  | toString Real_DIVIDE = "Real./"
  | toString Real_TILDE = "Real.~"
  | toString Real_LT = "Real.<"
  | toString Real_LE = "Real.<="
  | toString Real_GT = "Real.>"
  | toString Real_GE = "Real.>="
  | toString Char_EQUAL = "Char.="
  | toString Char_LT = "Char.<"
  | toString Char_LE = "Char.<="
  | toString Char_GT = "Char.>"
  | toString Char_GE = "Char.>="
  | toString WideChar_EQUAL = "WideChar.="
  | toString WideChar_LT = "WideChar.<"
  | toString WideChar_LE = "WideChar.<="
  | toString WideChar_GT = "WideChar.>"
  | toString WideChar_GE = "WideChar.>="
  | toString String_EQUAL = "String.="
  | toString String_LT = "String.<"
  | toString String_LE = "String.<="
  | toString String_GT = "String.>"
  | toString String_GE = "String.>="
  | toString String_HAT = "String.^"
  | toString String_size = "String.size"
  | toString String_str = "String.str"
  | toString WideString_EQUAL = "WideString.="
  | toString WideString_LT = "WideString.<"
  | toString WideString_LE = "WideString.<="
  | toString WideString_GT = "WideString.>"
  | toString WideString_GE = "WideString.>="
  | toString WideString_HAT = "WideString.^"
  | toString WideString_size = "WideString.size"
  | toString WideString_str = "WideString.str"
  | toString IntInf_EQUAL = "IntInf.="
  | toString IntInf_PLUS = "IntInf.+"
  | toString IntInf_MINUS = "IntInf.-"
  | toString IntInf_TIMES = "IntInf.*"
  | toString IntInf_TILDE = "IntInf.~"
  | toString IntInf_LT = "IntInf.<"
  | toString IntInf_LE = "IntInf.<="
  | toString IntInf_GT = "IntInf.>"
  | toString IntInf_GE = "IntInf.>="
  | toString IntInf_andb = "IntInf.andb"
  | toString IntInf_orb = "IntInf.orb"
  | toString IntInf_xorb = "IntInf.xorb"
  | toString IntInf_notb = "IntInf.notb"
  | toString IntInf_quot_unchecked = "IntInf.quot.unchecked"
  | toString IntInf_rem_unchecked = "IntInf.rem.unchecked"
  | toString Vector_length = "Vector.length"
  | toString Vector_unsafeFromListRevN = "Vector.unsafeFromListRevN"
  | toString Array_EQUAL = "Array.="
  | toString Array_length = "Array.length"
  | toString Unsafe_cast = "Unsafe.cast"
  | toString Unsafe_Vector_sub = "Unsafe.Vector.sub"
  | toString Unsafe_Array_sub = "Unsafe.Array.sub"
  | toString Unsafe_Array_update = "Unsafe.Array.update"
  | toString Exception_instanceof = "Exception.instanceof"
  | toString Cont_callcc = "Cont.callcc"
  | toString Cont_throw = "Cont.throw"
  | toString DelimCont_newPrompt = "DelimCont.newPrompt"
  | toString DelimCont_pushPrompt = "DelimCont.pushPrompt"
  | toString DelimCont_withSubCont = "DelimCont.withSubCont"
  | toString DelimCont_pushSubCont = "DelimCont.pushSubCont"
  | toString Lua_sub = "Lua.sub"
  | toString Lua_set = "Lua.set"
  | toString Lua_isNil = "Lua.isNil"
  | toString Lua_EQUAL = "Lua.=="
  | toString Lua_NOTEQUAL = "Lua.~="
  | toString Lua_LT = "Lua.<"
  | toString Lua_LE = "Lua.<="
  | toString Lua_GT = "Lua.>"
  | toString Lua_GE = "Lua.>="
  | toString Lua_PLUS = "Lua.+"
  | toString Lua_MINUS = "Lua.-"
  | toString Lua_TIMES = "Lua.*"
  | toString Lua_DIVIDE = "Lua./"
  | toString Lua_INTDIV = "Lua.//"
  | toString Lua_MOD = "Lua.%"
  | toString Lua_pow = "Lua.pow"
  | toString Lua_unm = "Lua.unm"
  | toString Lua_andb = "Lua.andb"
  | toString Lua_orb = "Lua.orb"
  | toString Lua_xorb = "Lua.xorb"
  | toString Lua_notb = "Lua.notb"
  | toString Lua_LSHIFT = "Lua.<<"
  | toString Lua_RSHIFT = "Lua.>>"
  | toString Lua_concat = "Lua.concat"
  | toString Lua_length = "Lua.length"
  | toString Lua_isFalsy = "Lua.isFalsy"
  | toString Lua_call0 = "Lua.call0"
  | toString Lua_call1 = "Lua.call1"
  | toString Lua_call2 = "Lua.call2"
  | toString Lua_call3 = "Lua.call3"
  | toString JavaScript_sub = "JavaScript.sub"
  | toString JavaScript_set = "JavaScript.set"
  | toString JavaScript_EQUAL = "JavaScript.==="
  | toString JavaScript_NOTEQUAL = "JavaScript.!=="
  | toString JavaScript_LT = "JavaScript.<"
  | toString JavaScript_LE = "JavaScript.<="
  | toString JavaScript_GT = "JavaScript.>"
  | toString JavaScript_GE = "JavaScript.>="
  | toString JavaScript_PLUS = "JavaScript.+"
  | toString JavaScript_MINUS = "JavaScript.-"
  | toString JavaScript_TIMES = "JavaScript.*"
  | toString JavaScript_DIVIDE = "JavaScript./"
  | toString JavaScript_MOD = "JavaScript.%"
  | toString JavaScript_negate = "JavaScript.negate"
  | toString JavaScript_andb = "JavaScript.andb"
  | toString JavaScript_orb = "JavaScript.orb"
  | toString JavaScript_xorb = "JavaScript.xorb"
  | toString JavaScript_notb = "JavaScript.notb"
  | toString JavaScript_LSHIFT = "JavaScript.<<"
  | toString JavaScript_RSHIFT = "JavaScript.>>"
  | toString JavaScript_URSHIFT = "JavaScript.>>>"
  | toString JavaScript_EXP = "JavaScript.**"
  | toString JavaScript_isFalsy = "JavaScript.isFalsy"
  | toString JavaScript_typeof = "JavaScript.typeof"
  | toString JavaScript_global = "JavaScript.global"
fun fromString "=" = SOME EQUAL
  | fromString "call2" = SOME call2
  | fromString "call3" = SOME call3
  | fromString "Ref.=" = SOME Ref_EQUAL
  | fromString "Ref.:=" = SOME Ref_set
  | fromString "Ref.!" = SOME Ref_read
  | fromString "Bool.=" = SOME Bool_EQUAL
  | fromString "Bool.not" = SOME Bool_not
  | fromString "Int.=" = SOME Int_EQUAL
  | fromString "Int.<" = SOME Int_LT
  | fromString "Int.<=" = SOME Int_LE
  | fromString "Int.>" = SOME Int_GT
  | fromString "Int.>=" = SOME Int_GE
  | fromString "Word.=" = SOME Word_EQUAL
  | fromString "Word.+" = SOME Word_PLUS
  | fromString "Word.-" = SOME Word_MINUS
  | fromString "Word.*" = SOME Word_TIMES
  | fromString "Word.~" = SOME Word_TILDE
  | fromString "Word.<" = SOME Word_LT
  | fromString "Word.<=" = SOME Word_LE
  | fromString "Word.>" = SOME Word_GT
  | fromString "Word.>=" = SOME Word_GE
  | fromString "Real.+" = SOME Real_PLUS
  | fromString "Real.-" = SOME Real_MINUS
  | fromString "Real.*" = SOME Real_TIMES
  | fromString "Real./" = SOME Real_DIVIDE
  | fromString "Real.~" = SOME Real_TILDE
  | fromString "Real.<" = SOME Real_LT
  | fromString "Real.<=" = SOME Real_LE
  | fromString "Real.>" = SOME Real_GT
  | fromString "Real.>=" = SOME Real_GE
  | fromString "Char.=" = SOME Char_EQUAL
  | fromString "Char.<" = SOME Char_LT
  | fromString "Char.<=" = SOME Char_LE
  | fromString "Char.>" = SOME Char_GT
  | fromString "Char.>=" = SOME Char_GE
  | fromString "WideChar.=" = SOME WideChar_EQUAL
  | fromString "WideChar.<" = SOME WideChar_LT
  | fromString "WideChar.<=" = SOME WideChar_LE
  | fromString "WideChar.>" = SOME WideChar_GT
  | fromString "WideChar.>=" = SOME WideChar_GE
  | fromString "String.=" = SOME String_EQUAL
  | fromString "String.<" = SOME String_LT
  | fromString "String.<=" = SOME String_LE
  | fromString "String.>" = SOME String_GT
  | fromString "String.>=" = SOME String_GE
  | fromString "String.^" = SOME String_HAT
  | fromString "String.size" = SOME String_size
  | fromString "String.str" = SOME String_str
  | fromString "WideString.=" = SOME WideString_EQUAL
  | fromString "WideString.<" = SOME WideString_LT
  | fromString "WideString.<=" = SOME WideString_LE
  | fromString "WideString.>" = SOME WideString_GT
  | fromString "WideString.>=" = SOME WideString_GE
  | fromString "WideString.^" = SOME WideString_HAT
  | fromString "WideString.size" = SOME WideString_size
  | fromString "WideString.str" = SOME WideString_str
  | fromString "IntInf.=" = SOME IntInf_EQUAL
  | fromString "IntInf.+" = SOME IntInf_PLUS
  | fromString "IntInf.-" = SOME IntInf_MINUS
  | fromString "IntInf.*" = SOME IntInf_TIMES
  | fromString "IntInf.~" = SOME IntInf_TILDE
  | fromString "IntInf.<" = SOME IntInf_LT
  | fromString "IntInf.<=" = SOME IntInf_LE
  | fromString "IntInf.>" = SOME IntInf_GT
  | fromString "IntInf.>=" = SOME IntInf_GE
  | fromString "IntInf.andb" = SOME IntInf_andb
  | fromString "IntInf.orb" = SOME IntInf_orb
  | fromString "IntInf.xorb" = SOME IntInf_xorb
  | fromString "IntInf.notb" = SOME IntInf_notb
  | fromString "IntInf.quot.unchecked" = SOME IntInf_quot_unchecked
  | fromString "IntInf.rem.unchecked" = SOME IntInf_rem_unchecked
  | fromString "Vector.length" = SOME Vector_length
  | fromString "Vector.unsafeFromListRevN" = SOME Vector_unsafeFromListRevN
  | fromString "Array.=" = SOME Array_EQUAL
  | fromString "Array.length" = SOME Array_length
  | fromString "Unsafe.cast" = SOME Unsafe_cast
  | fromString "Unsafe.Vector.sub" = SOME Unsafe_Vector_sub
  | fromString "Unsafe.Array.sub" = SOME Unsafe_Array_sub
  | fromString "Unsafe.Array.update" = SOME Unsafe_Array_update
  | fromString "Exception.instanceof" = SOME Exception_instanceof
  | fromString "Cont.callcc" = SOME Cont_callcc
  | fromString "Cont.throw" = SOME Cont_throw
  | fromString "DelimCont.newPrompt" = SOME DelimCont_newPrompt
  | fromString "DelimCont.pushPrompt" = SOME DelimCont_pushPrompt
  | fromString "DelimCont.withSubCont" = SOME DelimCont_withSubCont
  | fromString "DelimCont.pushSubCont" = SOME DelimCont_pushSubCont
  | fromString "Lua.sub" = SOME Lua_sub
  | fromString "Lua.set" = SOME Lua_set
  | fromString "Lua.isNil" = SOME Lua_isNil
  | fromString "Lua.==" = SOME Lua_EQUAL
  | fromString "Lua.~=" = SOME Lua_NOTEQUAL
  | fromString "Lua.<" = SOME Lua_LT
  | fromString "Lua.<=" = SOME Lua_LE
  | fromString "Lua.>" = SOME Lua_GT
  | fromString "Lua.>=" = SOME Lua_GE
  | fromString "Lua.+" = SOME Lua_PLUS
  | fromString "Lua.-" = SOME Lua_MINUS
  | fromString "Lua.*" = SOME Lua_TIMES
  | fromString "Lua./" = SOME Lua_DIVIDE
  | fromString "Lua.//" = SOME Lua_INTDIV
  | fromString "Lua.%" = SOME Lua_MOD
  | fromString "Lua.pow" = SOME Lua_pow
  | fromString "Lua.unm" = SOME Lua_unm
  | fromString "Lua.andb" = SOME Lua_andb
  | fromString "Lua.orb" = SOME Lua_orb
  | fromString "Lua.xorb" = SOME Lua_xorb
  | fromString "Lua.notb" = SOME Lua_notb
  | fromString "Lua.<<" = SOME Lua_LSHIFT
  | fromString "Lua.>>" = SOME Lua_RSHIFT
  | fromString "Lua.concat" = SOME Lua_concat
  | fromString "Lua.length" = SOME Lua_length
  | fromString "Lua.isFalsy" = SOME Lua_isFalsy
  | fromString "Lua.call0" = SOME Lua_call0
  | fromString "Lua.call1" = SOME Lua_call1
  | fromString "Lua.call2" = SOME Lua_call2
  | fromString "Lua.call3" = SOME Lua_call3
  | fromString "JavaScript.sub" = SOME JavaScript_sub
  | fromString "JavaScript.set" = SOME JavaScript_set
  | fromString "JavaScript.===" = SOME JavaScript_EQUAL
  | fromString "JavaScript.!==" = SOME JavaScript_NOTEQUAL
  | fromString "JavaScript.<" = SOME JavaScript_LT
  | fromString "JavaScript.<=" = SOME JavaScript_LE
  | fromString "JavaScript.>" = SOME JavaScript_GT
  | fromString "JavaScript.>=" = SOME JavaScript_GE
  | fromString "JavaScript.+" = SOME JavaScript_PLUS
  | fromString "JavaScript.-" = SOME JavaScript_MINUS
  | fromString "JavaScript.*" = SOME JavaScript_TIMES
  | fromString "JavaScript./" = SOME JavaScript_DIVIDE
  | fromString "JavaScript.%" = SOME JavaScript_MOD
  | fromString "JavaScript.negate" = SOME JavaScript_negate
  | fromString "JavaScript.andb" = SOME JavaScript_andb
  | fromString "JavaScript.orb" = SOME JavaScript_orb
  | fromString "JavaScript.xorb" = SOME JavaScript_xorb
  | fromString "JavaScript.notb" = SOME JavaScript_notb
  | fromString "JavaScript.<<" = SOME JavaScript_LSHIFT
  | fromString "JavaScript.>>" = SOME JavaScript_RSHIFT
  | fromString "JavaScript.>>>" = SOME JavaScript_URSHIFT
  | fromString "JavaScript.**" = SOME JavaScript_EXP
  | fromString "JavaScript.isFalsy" = SOME JavaScript_isFalsy
  | fromString "JavaScript.typeof" = SOME JavaScript_typeof
  | fromString "JavaScript.global" = SOME JavaScript_global
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
                          val exn : ty
                          val exntag : ty
                          val LuaValue : ty
                          val JavaScriptValue : ty
                          val refOf : ty -> ty
                          val listOf : ty -> ty
                          val vectorOf : ty -> ty
                          val arrayOf : ty -> ty
                          val pairOf : ty * ty -> ty
                          val tupleOf : ty list -> ty
                          val function1Of : ty * ty -> ty
                          val function2Of : ty * ty * ty -> ty
                          val function3Of : ty * ty * ty * ty -> ty
                          val contOf : ty -> ty
                          val promptOf : ty -> ty
                          val subcontOf : ty * ty -> ty
                          val IsEqType : constraint
                         ) : sig
                               val typeOf : Primitives.PrimOp -> { vars : (tv * constraint list) list, args : ty vector, result : ty }
                             end = struct
fun typeOf Primitives.EQUAL = { vars = [(tyVarEqA, [IsEqType])], args = vector [tyEqA, tyEqA], result = bool }
  | typeOf Primitives.call2 = { vars = [(tyVarA, []), (tyVarB, []), (tyVarC, [])], args = vector [function2Of (tyA, tyB, tyC), tyB, tyC], result = tyA }
  | typeOf Primitives.call3 = { vars = [(tyVarA, []), (tyVarB, []), (tyVarC, []), (tyVarD, [])], args = vector [function3Of (tyA, tyB, tyC, tyD), tyB, tyC, tyD], result = tyA }
  | typeOf Primitives.Ref_EQUAL = { vars = [(tyVarA, [])], args = vector [refOf (tyA), refOf (tyA)], result = bool }
  | typeOf Primitives.Ref_set = { vars = [(tyVarA, [])], args = vector [refOf (tyA), tyA], result = unit }
  | typeOf Primitives.Ref_read = { vars = [(tyVarA, [])], args = vector [refOf (tyA)], result = tyA }
  | typeOf Primitives.Bool_EQUAL = { vars = [], args = vector [bool, bool], result = bool }
  | typeOf Primitives.Bool_not = { vars = [], args = vector [bool], result = bool }
  | typeOf Primitives.Int_EQUAL = { vars = [], args = vector [int, int], result = bool }
  | typeOf Primitives.Int_LT = { vars = [], args = vector [int, int], result = bool }
  | typeOf Primitives.Int_LE = { vars = [], args = vector [int, int], result = bool }
  | typeOf Primitives.Int_GT = { vars = [], args = vector [int, int], result = bool }
  | typeOf Primitives.Int_GE = { vars = [], args = vector [int, int], result = bool }
  | typeOf Primitives.Word_EQUAL = { vars = [], args = vector [word, word], result = bool }
  | typeOf Primitives.Word_PLUS = { vars = [], args = vector [word, word], result = word }
  | typeOf Primitives.Word_MINUS = { vars = [], args = vector [word, word], result = word }
  | typeOf Primitives.Word_TIMES = { vars = [], args = vector [word, word], result = word }
  | typeOf Primitives.Word_TILDE = { vars = [], args = vector [word], result = word }
  | typeOf Primitives.Word_LT = { vars = [], args = vector [word, word], result = bool }
  | typeOf Primitives.Word_LE = { vars = [], args = vector [word, word], result = bool }
  | typeOf Primitives.Word_GT = { vars = [], args = vector [word, word], result = bool }
  | typeOf Primitives.Word_GE = { vars = [], args = vector [word, word], result = bool }
  | typeOf Primitives.Real_PLUS = { vars = [], args = vector [real, real], result = real }
  | typeOf Primitives.Real_MINUS = { vars = [], args = vector [real, real], result = real }
  | typeOf Primitives.Real_TIMES = { vars = [], args = vector [real, real], result = real }
  | typeOf Primitives.Real_DIVIDE = { vars = [], args = vector [real, real], result = real }
  | typeOf Primitives.Real_TILDE = { vars = [], args = vector [real], result = real }
  | typeOf Primitives.Real_LT = { vars = [], args = vector [real, real], result = bool }
  | typeOf Primitives.Real_LE = { vars = [], args = vector [real, real], result = bool }
  | typeOf Primitives.Real_GT = { vars = [], args = vector [real, real], result = bool }
  | typeOf Primitives.Real_GE = { vars = [], args = vector [real, real], result = bool }
  | typeOf Primitives.Char_EQUAL = { vars = [], args = vector [char, char], result = bool }
  | typeOf Primitives.Char_LT = { vars = [], args = vector [char, char], result = bool }
  | typeOf Primitives.Char_LE = { vars = [], args = vector [char, char], result = bool }
  | typeOf Primitives.Char_GT = { vars = [], args = vector [char, char], result = bool }
  | typeOf Primitives.Char_GE = { vars = [], args = vector [char, char], result = bool }
  | typeOf Primitives.WideChar_EQUAL = { vars = [], args = vector [wideChar, wideChar], result = bool }
  | typeOf Primitives.WideChar_LT = { vars = [], args = vector [wideChar, wideChar], result = bool }
  | typeOf Primitives.WideChar_LE = { vars = [], args = vector [wideChar, wideChar], result = bool }
  | typeOf Primitives.WideChar_GT = { vars = [], args = vector [wideChar, wideChar], result = bool }
  | typeOf Primitives.WideChar_GE = { vars = [], args = vector [wideChar, wideChar], result = bool }
  | typeOf Primitives.String_EQUAL = { vars = [], args = vector [string, string], result = bool }
  | typeOf Primitives.String_LT = { vars = [], args = vector [string, string], result = bool }
  | typeOf Primitives.String_LE = { vars = [], args = vector [string, string], result = bool }
  | typeOf Primitives.String_GT = { vars = [], args = vector [string, string], result = bool }
  | typeOf Primitives.String_GE = { vars = [], args = vector [string, string], result = bool }
  | typeOf Primitives.String_HAT = { vars = [], args = vector [string, string], result = string }
  | typeOf Primitives.String_size = { vars = [], args = vector [string], result = int }
  | typeOf Primitives.String_str = { vars = [], args = vector [char], result = string }
  | typeOf Primitives.WideString_EQUAL = { vars = [], args = vector [wideString, wideString], result = bool }
  | typeOf Primitives.WideString_LT = { vars = [], args = vector [wideString, wideString], result = bool }
  | typeOf Primitives.WideString_LE = { vars = [], args = vector [wideString, wideString], result = bool }
  | typeOf Primitives.WideString_GT = { vars = [], args = vector [wideString, wideString], result = bool }
  | typeOf Primitives.WideString_GE = { vars = [], args = vector [wideString, wideString], result = bool }
  | typeOf Primitives.WideString_HAT = { vars = [], args = vector [wideString, wideString], result = wideString }
  | typeOf Primitives.WideString_size = { vars = [], args = vector [wideString], result = int }
  | typeOf Primitives.WideString_str = { vars = [], args = vector [wideChar], result = wideString }
  | typeOf Primitives.IntInf_EQUAL = { vars = [], args = vector [intInf, intInf], result = bool }
  | typeOf Primitives.IntInf_PLUS = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.IntInf_MINUS = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.IntInf_TIMES = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.IntInf_TILDE = { vars = [], args = vector [intInf], result = intInf }
  | typeOf Primitives.IntInf_LT = { vars = [], args = vector [intInf, intInf], result = bool }
  | typeOf Primitives.IntInf_LE = { vars = [], args = vector [intInf, intInf], result = bool }
  | typeOf Primitives.IntInf_GT = { vars = [], args = vector [intInf, intInf], result = bool }
  | typeOf Primitives.IntInf_GE = { vars = [], args = vector [intInf, intInf], result = bool }
  | typeOf Primitives.IntInf_andb = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.IntInf_orb = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.IntInf_xorb = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.IntInf_notb = { vars = [], args = vector [intInf], result = intInf }
  | typeOf Primitives.IntInf_quot_unchecked = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.IntInf_rem_unchecked = { vars = [], args = vector [intInf, intInf], result = intInf }
  | typeOf Primitives.Vector_length = { vars = [(tyVarA, [])], args = vector [vectorOf (tyA)], result = int }
  | typeOf Primitives.Vector_unsafeFromListRevN = { vars = [(tyVarA, [])], args = vector [int, listOf (tyA)], result = vectorOf (tyA) }
  | typeOf Primitives.Array_EQUAL = { vars = [(tyVarA, [])], args = vector [arrayOf (tyA), arrayOf (tyA)], result = bool }
  | typeOf Primitives.Array_length = { vars = [(tyVarA, [])], args = vector [arrayOf (tyA)], result = int }
  | typeOf Primitives.Unsafe_cast = { vars = [(tyVarA, []), (tyVarB, [])], args = vector [tyA], result = tyB }
  | typeOf Primitives.Unsafe_Vector_sub = { vars = [(tyVarA, [])], args = vector [vectorOf (tyA), int], result = tyA }
  | typeOf Primitives.Unsafe_Array_sub = { vars = [(tyVarA, [])], args = vector [arrayOf (tyA), int], result = tyA }
  | typeOf Primitives.Unsafe_Array_update = { vars = [(tyVarA, [])], args = vector [arrayOf (tyA), int, tyA], result = unit }
  | typeOf Primitives.Exception_instanceof = { vars = [], args = vector [exn, exntag], result = bool }
  | typeOf Primitives.Cont_callcc = { vars = [(tyVarA, [])], args = vector [function1Of (tyA, contOf (tyA))], result = tyA }
  | typeOf Primitives.Cont_throw = { vars = [(tyVarA, []), (tyVarB, [])], args = vector [contOf (tyA)], result = function1Of (tyB, tyA) }
  | typeOf Primitives.DelimCont_newPrompt = { vars = [(tyVarA, [])], args = vector [], result = promptOf (tyA) }
  | typeOf Primitives.DelimCont_pushPrompt = { vars = [(tyVarA, [])], args = vector [promptOf (tyA), function1Of (tyA, unit)], result = tyA }
  | typeOf Primitives.DelimCont_withSubCont = { vars = [(tyVarA, []), (tyVarB, [])], args = vector [promptOf (tyB), function1Of (tyB, subcontOf (tyA, tyB))], result = tyA }
  | typeOf Primitives.DelimCont_pushSubCont = { vars = [(tyVarA, []), (tyVarB, [])], args = vector [subcontOf (tyA, tyB), function1Of (tyA, unit)], result = tyB }
  | typeOf Primitives.Lua_sub = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_set = { vars = [], args = vector [LuaValue, LuaValue, LuaValue], result = unit }
  | typeOf Primitives.Lua_isNil = { vars = [], args = vector [LuaValue], result = bool }
  | typeOf Primitives.Lua_EQUAL = { vars = [], args = vector [LuaValue, LuaValue], result = bool }
  | typeOf Primitives.Lua_NOTEQUAL = { vars = [], args = vector [LuaValue, LuaValue], result = bool }
  | typeOf Primitives.Lua_LT = { vars = [], args = vector [LuaValue, LuaValue], result = bool }
  | typeOf Primitives.Lua_LE = { vars = [], args = vector [LuaValue, LuaValue], result = bool }
  | typeOf Primitives.Lua_GT = { vars = [], args = vector [LuaValue, LuaValue], result = bool }
  | typeOf Primitives.Lua_GE = { vars = [], args = vector [LuaValue, LuaValue], result = bool }
  | typeOf Primitives.Lua_PLUS = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_MINUS = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_TIMES = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_DIVIDE = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_INTDIV = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_MOD = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_pow = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_unm = { vars = [], args = vector [LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_andb = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_orb = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_xorb = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_notb = { vars = [], args = vector [LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_LSHIFT = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_RSHIFT = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_concat = { vars = [], args = vector [LuaValue, LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_length = { vars = [], args = vector [LuaValue], result = LuaValue }
  | typeOf Primitives.Lua_isFalsy = { vars = [], args = vector [LuaValue], result = bool }
  | typeOf Primitives.Lua_call0 = { vars = [], args = vector [LuaValue, vectorOf (LuaValue)], result = unit }
  | typeOf Primitives.Lua_call1 = { vars = [], args = vector [LuaValue, vectorOf (LuaValue)], result = LuaValue }
  | typeOf Primitives.Lua_call2 = { vars = [], args = vector [LuaValue, vectorOf (LuaValue)], result = pairOf (LuaValue, LuaValue) }
  | typeOf Primitives.Lua_call3 = { vars = [], args = vector [LuaValue, vectorOf (LuaValue)], result = tupleOf [LuaValue, LuaValue, LuaValue] }
  | typeOf Primitives.JavaScript_sub = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_set = { vars = [], args = vector [JavaScriptValue, JavaScriptValue, JavaScriptValue], result = unit }
  | typeOf Primitives.JavaScript_EQUAL = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = bool }
  | typeOf Primitives.JavaScript_NOTEQUAL = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = bool }
  | typeOf Primitives.JavaScript_LT = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = bool }
  | typeOf Primitives.JavaScript_LE = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = bool }
  | typeOf Primitives.JavaScript_GT = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = bool }
  | typeOf Primitives.JavaScript_GE = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = bool }
  | typeOf Primitives.JavaScript_PLUS = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_MINUS = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_TIMES = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_DIVIDE = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_MOD = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_negate = { vars = [], args = vector [JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_andb = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_orb = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_xorb = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_notb = { vars = [], args = vector [JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_LSHIFT = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_RSHIFT = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_URSHIFT = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_EXP = { vars = [], args = vector [JavaScriptValue, JavaScriptValue], result = JavaScriptValue }
  | typeOf Primitives.JavaScript_isFalsy = { vars = [], args = vector [JavaScriptValue], result = bool }
  | typeOf Primitives.JavaScript_typeof = { vars = [], args = vector [JavaScriptValue], result = wideString }
  | typeOf Primitives.JavaScript_global = { vars = [], args = vector [wideString], result = JavaScriptValue }
end;
