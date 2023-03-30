(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure CodeGenLua = struct
exception CodeGenError of string

datatype target_lua_version = LUA5_3 | LUAJIT
type Context = { nextLuaId : int ref
               , targetLuaVersion : target_lua_version
               , hasDelimitedContinuations : bool
               }

val builtins
    = let open InitialEnv
      in List.foldl (fn ((vid, name), map) => TypedSyntax.VIdMap.insert (map, vid, name)) TypedSyntax.VIdMap.empty
                    [(* exn *)
                     (VId_Match, "_Match")
                    ,(VId_Bind, "_Bind")
                    ,(VId_Div, "_Div")
                    ,(VId_Overflow, "_Overflow")
                    ,(VId_Size, "_Size")
                    ,(VId_Subscript, "_Subscript")
                    ,(VId_Fail, "_Fail")
                    ,(VId_Match_tag, "_Match_tag")
                    ,(VId_Bind_tag, "_Bind_tag")
                    ,(VId_Div_tag, "_Div_tag")
                    ,(VId_Overflow_tag, "_Overflow_tag")
                    ,(VId_Size_tag, "_Size_tag")
                    ,(VId_Subscript_tag, "_Subscript_tag")
                    ,(VId_Fail_tag, "_Fail_tag")
                    ,(VId_exnName, "_exnName")
                    (* Overloaded: VId_abs, VId_TILDE, VId_div, VId_mod, VId_TIMES, VId_DIVIDE, VId_PLUS, VId_MINUS, VId_LT, VId_GT, VId_LE, VId_GE *)
                    (* int *)
                    ,(VId_Int_abs, "_Int_abs") (* may raise Overflow *)
                    ,(VId_Int_TILDE, "_Int_negate") (* may raise Overflow *)
                    (* real *)
                    ,(VId_Real_abs, "math_abs") (* Lua math.abs *)
                    (* Vector and Array *)
                    ,(VId_Vector_tabulate, "_VectorOrArray_tabulate")
                    ,(VId_Vector_concat, "_Vector_concat")
                    ,(VId_Vector_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_array, "_Array_array")
                    ,(VId_Array_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_tabulate, "_VectorOrArray_tabulate")
                    (* Delimited continuations *)
                    ,(VId_DelimCont_pushPrompt, "_pushPrompt")
                    ,(VId_DelimCont_withSubCont, "_withSubCont")
                    ,(VId_DelimCont_pushSubCont, "_pushSubCont")
                    (* Lua interface *)
                    ,(VId_Lua_LuaError, "_LuaError")
                    ,(VId_Lua_LuaError_tag, "_LuaError_tag")
                    ,(VId_Lua_NIL, "nil") (* literal *)
                    ,(VId_Lua_function, "_Lua_function")
                    ,(VId_Lua_Lib_assert, "assert")
                    ,(VId_Lua_Lib_error, "error")
                    ,(VId_Lua_Lib_getmetatable, "getmetatable")
                    ,(VId_Lua_Lib_pairs, "pairs")
                    ,(VId_Lua_Lib_pcall, "pcall")
                    ,(VId_Lua_Lib_setmetatable, "setmetatable")
                    ,(VId_Lua_Lib_math, "math")
                    ,(VId_Lua_Lib_math_abs, "math_abs")
                    ,(VId_Lua_Lib_math_type, "math_type")
                    ,(VId_Lua_Lib_math_maxinteger, "math_maxinteger")
                    ,(VId_Lua_Lib_math_mininteger, "math_mininteger")
                    ,(VId_Lua_Lib_string, "string")
                    ,(VId_Lua_Lib_string_char, "string_char")
                    ,(VId_Lua_Lib_string_format, "string_format")
                    ,(VId_Lua_Lib_table, "table")
                    ,(VId_Lua_Lib_table_pack, "table_pack")
                    ,(VId_Lua_Lib_table_unpack, "table_unpack")
                    ]
      end
val builtinsLuaJIT
    = let open InitialEnv
      in List.foldl (fn ((vid, name), map) => TypedSyntax.VIdMap.insert (map, vid, name)) TypedSyntax.VIdMap.empty
                    [(* exn *)
                     (VId_Match, "_Match")
                    ,(VId_Bind, "_Bind")
                    ,(VId_Div, "_Div")
                    ,(VId_Overflow, "_Overflow")
                    ,(VId_Size, "_Size")
                    ,(VId_Subscript, "_Subscript")
                    ,(VId_Fail, "_Fail")
                    ,(VId_Match_tag, "_Match_tag")
                    ,(VId_Bind_tag, "_Bind_tag")
                    ,(VId_Div_tag, "_Div_tag")
                    ,(VId_Overflow_tag, "_Overflow_tag")
                    ,(VId_Size_tag, "_Size_tag")
                    ,(VId_Subscript_tag, "_Subscript_tag")
                    ,(VId_Fail_tag, "_Fail_tag")
                    ,(VId_exnName, "_exnName")
                    (* Overloaded: VId_abs, VId_TILDE, VId_div, VId_mod, VId_TIMES, VId_DIVIDE, VId_PLUS, VId_MINUS, VId_LT, VId_GT, VId_LE, VId_GE *)
                    (* int *)
                    ,(VId_Int_abs, "_Int_abs") (* may raise Overflow *)
                    ,(VId_Int_TILDE, "_Int_negate") (* may raise Overflow *)
                    (* real *)
                    ,(VId_Real_abs, "math_abs") (* Lua math.abs *)
                    (* Vector and Array *)
                    ,(VId_Vector_tabulate, "_VectorOrArray_tabulate")
                    ,(VId_Vector_concat, "_Vector_concat")
                    ,(VId_Vector_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_array, "_Array_array")
                    ,(VId_Array_fromList, "_VectorOrArray_fromList")
                    ,(VId_Array_tabulate, "_VectorOrArray_tabulate")
                    (* Lua interface *)
                    ,(VId_Lua_LuaError, "_LuaError")
                    ,(VId_Lua_LuaError_tag, "_LuaError_tag")
                    ,(VId_Lua_NIL, "nil") (* literal *)
                    ,(VId_Lua_function, "_Lua_function")
                    ,(VId_Lua_Lib_assert, "assert")
                    ,(VId_Lua_Lib_error, "error")
                    ,(VId_Lua_Lib_getmetatable, "getmetatable")
                    ,(VId_Lua_Lib_pairs, "pairs")
                    ,(VId_Lua_Lib_pcall, "pcall")
                    ,(VId_Lua_Lib_setmetatable, "setmetatable")
                    ,(VId_Lua_Lib_math, "math")
                    ,(VId_Lua_Lib_math_abs, "math_abs")
                    ,(VId_Lua_Lib_string, "string")
                    ,(VId_Lua_Lib_string_char, "string_char")
                    ,(VId_Lua_Lib_string_format, "string_format")
                    ,(VId_Lua_Lib_table, "table")
                    ,(VId_Lua_Lib_table_pack, "table_pack")
                    ,(VId_Lua_Lib_table_unpack, "table_unpack")
                    ,(VId_Lua_Lib_bit, "bit")
                    ,(VId_Lua_Lib_bit_bnot, "bit_bnot")
                    ,(VId_Lua_Lib_bit_band, "bit_band")
                    ,(VId_Lua_Lib_bit_bor, "bit_bor")
                    ,(VId_Lua_Lib_bit_bxor, "bit_bxor")
                    ,(VId_Lua_Lib_bit_lshift, "bit_lshift")
                    ,(VId_Lua_Lib_bit_rshift, "bit_rshift")
                    ]
      end
fun VIdToLua (ctx : Context, vid as TypedSyntax.MkVId (name, n))
    = if n < 0 then
          case #targetLuaVersion ctx of
              LUA5_3 => (case TypedSyntax.VIdMap.find (builtins, vid) of
                             NONE => raise Fail ("the built-in symbol " ^ name ^ "@" ^ Int.toString n ^ " is not supported by Lua backend")
                           | SOME luaName => LuaSyntax.PredefinedId luaName
                        )
            | LUAJIT => (case TypedSyntax.VIdMap.find (builtinsLuaJIT, vid) of
                             NONE => raise Fail ("the built-in symbol " ^ name ^ "@" ^ Int.toString n ^ " is not supported by LuaJIT backend")
                           | SOME luaName => LuaSyntax.PredefinedId luaName
                        )
      else
          LuaSyntax.UserDefinedId vid

fun LabelToTableKey (Syntax.NumericLabel n) = LuaSyntax.IntKey n
  | LabelToTableKey (Syntax.IdentifierLabel s) = LuaSyntax.StringKey s

fun genSym (ctx: Context) = let val n = !(#nextLuaId ctx)
                                val _ = #nextLuaId ctx := n + 1
                            in TypedSyntax.MkVId ("tmp", n)
                            end
fun genSymWithName (ctx : Context, name : string)
    = let val n = !(#nextLuaId ctx)
          val _ = #nextLuaId ctx := n + 1
      in TypedSyntax.MkVId (name, n)
      end

structure F = FSyntax
structure C = CSyntax
structure L = struct
fun ConstStat (vid : TypedSyntax.VId, e : LuaSyntax.Exp) = LuaSyntax.LocalStat ([(vid, LuaSyntax.CONST)], [e])
open LuaSyntax
end

fun ExpStat (L.CallExp (f, args)) = [L.CallStat (f, args)]
  | ExpStat (L.MethodExp (self, name, args)) = [L.MethodStat (self, name, args)]
  | ExpStat (L.ConstExp _) = []
  | ExpStat e = [L.CallStat (L.VarExp (L.PredefinedId "_id"), vector [e])]

datatype cont_type = GOTO of { label : L.Id, params : L.Id list }
                   | RETURN
type Env = { continuations : cont_type C.CVarMap.map
           }

datatype purity = PURE | DISCARDABLE | IMPURE

fun applyCont (ctx : Context, env : Env, cont : C.CVar, args : L.Exp list)
    = case C.CVarMap.find (#continuations env, cont) of
          SOME (GOTO { label, params = [] }) => [L.GotoStat label]
        | SOME (GOTO { label, params }) => [L.AssignStat (List.map L.VarExp params, args), L.GotoStat label]
        | SOME RETURN => [L.ReturnStat (vector args)]
        | NONE => raise CodeGenError "undefined continuation"

fun doLabel cname = L.UserDefinedId (TypedSyntax.MkVId ("cont", C.CVar.toInt cname))

fun doValue ctx (C.Var vid) = (case VIdToLua (ctx, vid) of
                                   L.PredefinedId "nil" => L.ConstExp L.Nil
                                 | L.PredefinedId "false" => L.ConstExp L.False
                                 | L.PredefinedId "true" => L.ConstExp L.True
                                 | id => L.VarExp id
                              )
  | doValue ctx C.Unit = L.ConstExp L.Nil
  | doValue ctx C.Nil = L.ConstExp L.Nil (* empty list *)
  | doValue ctx (C.BoolConst false) = L.ConstExp L.False
  | doValue ctx (C.BoolConst true) = L.ConstExp L.True
  | doValue ctx (C.NativeIntConst x) = if x < 0 then
                                           if x = ~0x8000000000000000 then
                                               L.BinExp (L.MINUS, L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (LargeInt.toString (~ (x + 1))))), L.ConstExp (L.Numeral "1"))
                                           else
                                               L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (LargeInt.toString (~ x))))
                                       else
                                           L.ConstExp (L.Numeral (LargeInt.toString x))
  | doValue ctx (C.Int32Const x) = if x < 0 then
                                       L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (LargeInt.toString (~ (Int32.toLarge x)))))
                                   else
                                       L.ConstExp (L.Numeral (Int32.toString x))
  | doValue ctx (C.Int54Const x) = if x < 0 then
                                       L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (Int64.toString (~ x))))
                                   else
                                       L.ConstExp (L.Numeral (Int64.toString x))
  | doValue ctx (C.Int64Const x) = let val suffix = case #targetLuaVersion ctx of
                                                        LUA5_3 => ""
                                                      | LUAJIT => "LL"
                                   in if x < 0 then
                                          if x = ~0x8000000000000000 then
                                              L.BinExp (L.MINUS, L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (Int64.toString (~ (x + 1)) ^ suffix))), L.ConstExp (L.Numeral ("1" ^ suffix)))
                                          else
                                              L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (Int64.toString (~ x) ^ suffix)))
                                      else
                                          L.ConstExp (L.Numeral (Int64.toString x ^ suffix))
                                   end
  | doValue ctx (C.IntInfConst x) = raise CodeGenError "IntInfConst is not supported by Lua backend"
  | doValue ctx (C.NativeWordConst x) = L.ConstExp (L.Numeral ("0x" ^ LargeInt.fmt StringCvt.HEX x))
  | doValue ctx (C.Word32Const x) = L.ConstExp (L.Numeral ("0x" ^ Word32.toString x))
  | doValue ctx (C.Word64Const x) = let val suffix = case #targetLuaVersion ctx of
                                                         LUA5_3 => ""
                                                       | LUAJIT => "ULL"
                                    in L.ConstExp (L.Numeral ("0x" ^ Word64.toString x ^ suffix))
                                    end
  | doValue ctx (C.CharConst c) = L.ConstExp (L.Numeral (Int.toString (Char.ord c)))
  | doValue ctx (C.Char16Const _) = raise CodeGenError "Char16Const is not supported by Lua backend"
  | doValue ctx (C.StringConst s) = L.ConstExp (L.LiteralString s)
  | doValue ctx (C.String16Const _) = raise CodeGenError "String16Const is not supported by Lua backend"

(* doDecs : Context * Env * C.Dec VectorSlice.slice * C.CExp * L.Stat list -> L.Stat list *)
fun doDecs (ctx, env, decs, finalExp, revStats : L.Stat list)
    = (case VectorSlice.getItem decs of
           NONE => List.revAppend (revStats, doCExp ctx env finalExp)
         | SOME (dec, decs) =>
           let fun pure (NONE, exp) = doDecs (ctx, env, decs, finalExp, revStats)
                 | pure (SOME result, exp) = doDecs (ctx, env, decs, finalExp, L.ConstStat (result, exp) :: revStats)
               fun discardable (NONE, exp) = doDecs (ctx, env, decs, finalExp, revStats)
                 | discardable (SOME result, exp) = doDecs (ctx, env, decs, finalExp, L.ConstStat (result, exp) :: revStats)
               fun impure (NONE, exp) = doDecs (ctx, env, decs, finalExp, List.revAppend (ExpStat exp, revStats))
                 | impure (SOME result, exp) = doDecs (ctx, env, decs, finalExp, L.ConstStat (result, exp) :: revStats)
               fun action (NONE, stmt) = doDecs (ctx, env, decs, finalExp, stmt :: revStats)
                 | action (SOME result, stmt) = doDecs (ctx, env, decs, finalExp, L.ConstStat (result, L.ConstExp L.Nil) :: stmt :: revStats)
           in case dec of
                  C.ValDec { exp = C.PrimOp { primOp = F.RealConstOp x, tyargs = _, args = _ }, result } =>
                  let val exp = if Numeric.Notation.isNegative x then
                                    case (#targetLuaVersion ctx, Numeric.Notation.isNegativeZero x) of
                                        (LUAJIT, true) => L.VarExp (L.PredefinedId "NEGATIVE_ZERO")
                                      | _ => L.UnaryExp (L.NEGATE, L.ConstExp (L.Numeral (Numeric.Notation.toString "-" (Numeric.Notation.abs x))))
                                else
                                    L.ConstExp (L.Numeral (Numeric.Notation.toString "-" x))
                  in pure (result, exp)
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.ListOp, tyargs = _, args = [] }, result } =>
                  pure (result, L.ConstExp L.Nil)
                | C.ValDec { exp = C.PrimOp { primOp = F.ListOp, tyargs = _, args = xs }, result } =>
                  let fun doFields (i, []) = []
                        | doFields (i, y :: ys) = (L.IntKey i, doValue ctx y) :: doFields (i + 1, ys)
                  in pure (result, L.CallExp (L.VarExp (L.PredefinedId "_list"), vector [L.TableExp (vector ((L.StringKey "n", L.ConstExp (L.Numeral (Int.toString (List.length xs)))) :: doFields (1, xs)))]))
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.VectorOp, tyargs = _, args = xs }, result } =>
                  let fun doFields (i, []) = []
                        | doFields (i, y :: ys) = (L.IntKey i, doValue ctx y) :: doFields (i + 1, ys)
                  in pure (result, L.TableExp (vector ((L.StringKey "n", L.ConstExp (L.Numeral (Int.toString (List.length xs)))) :: doFields (1, xs))))
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.DataTagAsStringOp info, tyargs = _, args = [exp] }, result } =>
                  pure (result, L.IndexExp (doValue ctx exp, L.ConstExp (L.LiteralString "tag")))
                | C.ValDec { exp = C.PrimOp { primOp = F.DataPayloadOp info, tyargs = _, args = [exp] }, result } =>
                  pure (result, L.IndexExp (doValue ctx exp, L.ConstExp (L.LiteralString "payload")))
                | C.ValDec { exp = C.PrimOp { primOp = F.ExnPayloadOp, tyargs = _, args = [exp] }, result } =>
                  pure (result, L.IndexExp (doValue ctx exp, L.ConstExp (L.LiteralString "payload")))
                | C.ValDec { exp = C.PrimOp { primOp = F.ConstructValOp info, tyargs = _, args = [] }, result } =>
                  let val tag = #tag info
                  in pure (result, L.TableExp (vector [(L.StringKey "tag", L.ConstExp (L.LiteralString tag))]))
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.ConstructValWithPayloadOp info, tyargs = _, args = [payload] }, result } =>
                  let val tag = #tag info
                      val payload = doValue ctx payload
                  in pure (result, L.TableExp (vector [(L.StringKey "tag", L.ConstExp (L.LiteralString tag)), (L.StringKey "payload", payload)]))
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.ConstructExnOp, tyargs = _, args = [tag] }, result } =>
                  let val tag = doValue ctx tag
                  in pure (result, L.TableExp (vector [(L.StringKey "tag", tag)]))
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.ConstructExnWithPayloadOp, tyargs = _, args = [tag, payload] }, result } =>
                  let val tag = doValue ctx tag
                      val payload = doValue ctx payload
                  in pure (result, L.TableExp (vector [(L.StringKey "tag", tag), (L.StringKey "payload", payload)]))
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.RaiseOp (span as { start as { file, line, column }, ... }), tyargs = _, args = [exp] }, result } =>
                  let val exp = doValue ctx exp
                      val locationInfo = if start = SourcePos.nullPos then
                                             L.ConstExp L.Nil
                                         else
                                             L.ConstExp (L.LiteralString (OS.Path.file file ^ ":" ^ Int.toString line ^ ":" ^ Int.toString column))
                  in List.rev (L.CallStat (L.VarExp (L.PredefinedId "_raise"), vector [exp, locationInfo]) :: revStats) (* discard continuation *)
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.PrimFnOp prim, tyargs, args }, result } =>
                  let fun doUnary f = case args of
                                          [a] => f (doValue ctx a)
                                        | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
                      fun doUnaryExp (f, purity) = doUnary (fn a =>
                                                               case purity of
                                                                   PURE => pure (result, f a)
                                                                 | DISCARDABLE => discardable (result, f a)
                                                                 | IMPURE => impure (result, f a)
                                                           )
                      fun doBinaryRaw f = case args of
                                              [a, b] => f (a, b)
                                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
                      fun doBinary f = doBinaryRaw (fn (a, b) => f (doValue ctx a, doValue ctx b))
                      fun doBinaryExpRaw (f, purity) = doBinaryRaw (fn (a, b) =>
                                                                       case purity of
                                                                           PURE => pure (result, f (a, b))
                                                                         | DISCARDABLE => discardable (result, f (a, b))
                                                                         | IMPURE => impure (result, f (a, b))
                                                                   )
                      fun doBinaryExp (f, purity) = doBinary (fn (a, b) =>
                                                                 case purity of
                                                                     PURE => pure (result, f (a, b))
                                                                   | DISCARDABLE => discardable (result, f (a, b))
                                                                   | IMPURE => impure (result, f (a, b))
                                                             )
                      fun doBinaryOp (binop, purity) = doBinaryExp (fn (a, b) => L.BinExp (binop, a, b), purity)
                      fun doTernary f = case args of
                                            [a, b, c] => f (doValue ctx a, doValue ctx b, doValue ctx c)
                                          | _ => raise CodeGenError ("primop " ^ Primitives.toString prim ^ ": invalid number of arguments")
                  in case prim of
                         Primitives.call2 => doTernary (fn (f, a0, a1) =>
                                                           impure (result, L.CallExp (f, vector [a0, a1]))
                                                       )
                       | Primitives.List_cons => doBinaryExp (fn (x, xs) => L.TableExp (vector [(L.IntKey 1, x), (L.IntKey 2, xs)]), PURE)
                       | Primitives.List_null => doUnaryExp (fn a => L.BinExp (L.EQUAL, a, L.ConstExp L.Nil), PURE)
                       | Primitives.List_unsafeHead => doUnaryExp (fn xs => L.IndexExp (xs, L.ConstExp (L.Numeral "1")), PURE)
                       | Primitives.List_unsafeTail => doUnaryExp (fn xs => L.IndexExp (xs, L.ConstExp (L.Numeral "2")), PURE)
                       | Primitives.Ref_ref => doUnaryExp ( fn x =>
                                                               (* REPRESENTATION_OF_REF *)
                                                               L.TableExp (vector [(L.IntKey 1, x)])
                                                          , DISCARDABLE
                                                          )
                       | Primitives.Ref_EQUAL => doBinaryOp (L.EQUAL, PURE)
                       | Primitives.Ref_set => doBinary (fn (a, b) =>
                                                            (* REPRESENTATION_OF_REF *)
                                                            action (result, L.AssignStat ([L.IndexExp (a, L.ConstExp (L.Numeral "1"))], [b]))
                                                        )
                       | Primitives.Ref_read => doUnaryExp ( fn a =>
                                                                (* REPRESENTATION_OF_REF *)
                                                                L.IndexExp (a, L.ConstExp (L.Numeral "1"))
                                                           , DISCARDABLE
                                                           )
                       | Primitives.Bool_EQUAL => doBinaryOp (L.EQUAL, PURE)
                       | Primitives.Bool_not => doUnaryExp (fn a => L.UnaryExp (L.NOT, a), PURE)
                       | Primitives.Int_EQUAL _ => doBinaryOp (L.EQUAL, PURE)
                       | Primitives.Int_PLUS i =>
                         (case (#targetLuaVersion ctx, i) of
                              (LUA5_3, Primitives.INT) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Int_add"), vector [a, b]), IMPURE)
                            | (LUAJIT, Primitives.I32) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Int_add"), vector [a, b]), IMPURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Int_MINUS i =>
                         (case (#targetLuaVersion ctx, i) of
                              (LUA5_3, Primitives.INT) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Int_sub"), vector [a, b]), IMPURE)
                            | (LUAJIT, Primitives.I32) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Int_sub"), vector [a, b]), IMPURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Int_TIMES i =>
                         (case (#targetLuaVersion ctx, i) of
                              (LUA5_3, Primitives.INT) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Int_mul"), vector [a, b]), IMPURE)
                            | (LUAJIT, Primitives.I32) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Int_mul"), vector [a, b]), IMPURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Int_div i =>
                         (case (#targetLuaVersion ctx, i) of
                              (LUA5_3, Primitives.INT) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Int_div"), vector [a, b]), IMPURE)
                            | (LUAJIT, Primitives.I32) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Int_div"), vector [a, b]), IMPURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Int_div_unchecked i =>
                         (case (#targetLuaVersion ctx, i) of
                              (LUA5_3, Primitives.INT) => doBinaryOp (L.INTDIV, PURE)
                            | (LUAJIT, Primitives.I32) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "math_floor"), vector [L.BinExp (L.DIV, a, b)]), PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Int_mod i =>
                         (case (#targetLuaVersion ctx, i) of
                              (LUA5_3, Primitives.INT) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Int_mod"), vector [a, b]), IMPURE)
                            | (LUAJIT, Primitives.I32) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Int_mod"), vector [a, b]), IMPURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Int_mod_unchecked i =>
                         (case (#targetLuaVersion ctx, i) of
                              (LUA5_3, Primitives.INT) => doBinaryOp (L.MOD, PURE)
                            | (LUAJIT, Primitives.I32) => doBinaryOp (L.MOD, PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Int_quot i =>
                         (case (#targetLuaVersion ctx, i) of
                              (LUA5_3, Primitives.INT) => raise CodeGenError "primop Int.quot is not supported on this target"
                            | (LUAJIT, Primitives.I32) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Int_quot"), vector [a, b]), IMPURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Int_quot_unchecked i =>
                         (case (#targetLuaVersion ctx, i) of
                              (LUA5_3, Primitives.INT) => raise CodeGenError "primop Int.quot.unchecked is not supported on this target"
                            | (LUAJIT, Primitives.I32) => doBinaryExp (fn (a, b) => L.SingleValueExp (L.CallExp (L.VarExp (L.PredefinedId "math_modf"), vector [L.BinExp (L.DIV, a, b)])), IMPURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Int_TILDE i =>
                         (case (#targetLuaVersion ctx, i) of
                              (LUA5_3, Primitives.INT) => doUnaryExp (fn a => L.CallExp (L.VarExp (L.PredefinedId "_Int_negate"), vector [a]), IMPURE)
                            | (LUAJIT, Primitives.I32) => doUnaryExp (fn a => L.CallExp (L.VarExp (L.PredefinedId "_Int_negate"), vector [a]), IMPURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Int_TILDE_unchecked i =>
                         (case (#targetLuaVersion ctx, i) of
                              (LUA5_3, Primitives.INT) => doUnaryExp (fn a => L.UnaryExp (L.NEGATE, a), IMPURE)
                            | (LUAJIT, Primitives.I32) => doUnaryExp (fn a => L.BinExp (L.MINUS, L.ConstExp (L.Numeral "0"), a), IMPURE) (* Should we avoid negative zero? *)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Int_abs i =>
                         (case (#targetLuaVersion ctx, i) of
                              (LUA5_3, Primitives.INT) => doUnaryExp (fn a => L.CallExp (L.VarExp (L.PredefinedId "_Int_abs"), vector [a]), IMPURE)
                            | (LUAJIT, Primitives.I32) => doUnaryExp (fn a => L.CallExp (L.VarExp (L.PredefinedId "_Int_abs"), vector [a]), IMPURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Int_LT _ => doBinaryOp (L.LT, PURE)
                       | Primitives.Int_GT _ => doBinaryOp (L.GT, PURE)
                       | Primitives.Int_LE _ => doBinaryOp (L.LE, PURE)
                       | Primitives.Int_GE _ => doBinaryOp (L.GE, PURE)
                       | Primitives.Word_EQUAL w => doBinaryOp (L.EQUAL, PURE)
                       | Primitives.Word_PLUS w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryOp (L.PLUS, PURE)
                            | (LUAJIT, Primitives.W32) => doBinaryExp (fn (a, b) => L.BinExp (L.MOD, L.BinExp (L.PLUS, a, b), L.ConstExp (L.Numeral "0x100000000")), PURE)
                            | (LUAJIT, Primitives.W64) => doBinaryOp (L.PLUS, PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_MINUS w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryOp (L.MINUS, PURE)
                            | (LUAJIT, Primitives.W32) => doBinaryExp (fn (a, b) => L.BinExp (L.MOD, L.BinExp (L.MINUS, a, b), L.ConstExp (L.Numeral "0x100000000")), PURE)
                            | (LUAJIT, Primitives.W64) => doBinaryOp (L.MINUS, PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_TIMES w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryOp (L.TIMES, PURE)
                            | (LUAJIT, Primitives.W32) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Word_mul"), vector [a, b]), PURE)
                            | (LUAJIT, Primitives.W64) => doBinaryOp (L.TIMES, PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_TILDE w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doUnaryExp (fn a => L.UnaryExp (L.NEGATE, a), PURE)
                            | (LUAJIT, Primitives.W32) => doUnaryExp (fn a => L.BinExp (L.MOD, L.UnaryExp (L.NEGATE, a), L.ConstExp (L.Numeral "0x100000000")), PURE)
                            | (LUAJIT, Primitives.W64) => doUnaryExp (fn a => L.UnaryExp (L.NEGATE, a), PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_div w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Word_div"), vector [a, b]), IMPURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_mod w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Word_mod"), vector [a, b]), IMPURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_div_unchecked w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryExpRaw ( fn (a, b as C.NativeWordConst b') => if b' mod 2 = 0 then
                                                                                                                     let fun shiftAmount (0, amount) = (0, amount) (* should not occur *)
                                                                                                                           | shiftAmount (n, amount) = if n mod 2 = 0 then
                                                                                                                                                           shiftAmount (n div 2, amount + 1)
                                                                                                                                                       else
                                                                                                                                                           (n, amount)
                                                                                                                         val (d, shift) = shiftAmount (b', 0)
                                                                                                                         val a' = L.BinExp (L.RSHIFT, doValue ctx a, L.ConstExp (L.Numeral (Int.toString shift)))
                                                                                                                     in if d = 1 then
                                                                                                                            a'
                                                                                                                        else
                                                                                                                            L.BinExp (L.INTDIV, a', L.ConstExp (L.Numeral (IntInf.toString d)))
                                                                                                                     end
                                                                                                                 else
                                                                                                                     L.CallExp (L.VarExp (L.PredefinedId "_Word_div"), vector [doValue ctx a, doValue ctx b])
                                                                          | (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Word_div"), vector [doValue ctx a, doValue ctx b])
                                                                          , PURE
                                                                          )
                            | (LUAJIT, Primitives.W32) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "math_floor"), vector [L.BinExp (L.DIV, a, b)]), PURE)
                            | (LUAJIT, Primitives.W64) => doBinaryOp (L.DIV, PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_mod_unchecked w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Word_mod"), vector [a, b]), PURE)
                            | (LUAJIT, Primitives.W32) => doBinaryOp (L.MOD, PURE)
                            | (LUAJIT, Primitives.W64) => doBinaryOp (L.MOD, PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_LT w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "math_ult"), vector [a, b]), PURE)
                            | (LUAJIT, Primitives.W32) => doBinaryOp (L.LT, PURE)
                            | (LUAJIT, Primitives.W64) => doBinaryOp (L.LT, PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_LE w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryExp (fn (a, b) => L.UnaryExp (L.NOT, L.CallExp (L.VarExp (L.PredefinedId "math_ult"), vector [b, a])), PURE)
                            | (LUAJIT, Primitives.W32) => doBinaryOp (L.LE, PURE)
                            | (LUAJIT, Primitives.W64) => doBinaryOp (L.LE, PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_GT w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "math_ult"), vector [b, a]), PURE)
                            | (LUAJIT, Primitives.W32) => doBinaryOp (L.GT, PURE)
                            | (LUAJIT, Primitives.W64) => doBinaryOp (L.GT, PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_GE w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryExp (fn (a, b) => L.UnaryExp (L.NOT, L.CallExp (L.VarExp (L.PredefinedId "math_ult"), vector [a, b])), PURE)
                            | (LUAJIT, Primitives.W32) => doBinaryOp (L.GE, PURE)
                            | (LUAJIT, Primitives.W64) => doBinaryOp (L.GE, PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_notb w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doUnaryExp (fn a => L.UnaryExp (L.BITNOT, a), PURE)
                            | (LUAJIT, Primitives.W32) => doUnaryExp (fn a => L.BinExp (L.MOD, L.CallExp (L.VarExp (L.PredefinedId "bit_bnot"), vector [a]), L.ConstExp (L.Numeral "0x100000000")), PURE)
                            | (LUAJIT, Primitives.W64) => doUnaryExp (fn a => L.CallExp (L.VarExp (L.PredefinedId "bit_bnot"), vector [a]), PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_andb w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryOp (L.BITAND, PURE)
                            | (LUAJIT, Primitives.W32) => doBinaryExp (fn (a, b) => L.BinExp (L.MOD, L.CallExp (L.VarExp (L.PredefinedId "bit_band"), vector [a, b]), L.ConstExp (L.Numeral "0x100000000")), PURE)
                            | (LUAJIT, Primitives.W64) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "bit_band"), vector [a, b]), PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_orb w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryOp (L.BITOR, PURE)
                            | (LUAJIT, Primitives.W32) => doBinaryExp (fn (a, b) => L.BinExp (L.MOD, L.CallExp (L.VarExp (L.PredefinedId "bit_bor"), vector [a, b]), L.ConstExp (L.Numeral "0x100000000")), PURE)
                            | (LUAJIT, Primitives.W64) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "bit_bor"), vector [a, b]), PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_xorb w =>
                         (case (#targetLuaVersion ctx, w) of
                              (LUA5_3, Primitives.WORD) => doBinaryOp (L.BITXOR, PURE)
                            | (LUAJIT, Primitives.W32) => doBinaryExp (fn (a, b) => L.BinExp (L.MOD, L.CallExp (L.VarExp (L.PredefinedId "bit_bxor"), vector [a, b]), L.ConstExp (L.Numeral "0x100000000")), PURE)
                            | (LUAJIT, Primitives.W64) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "bit_bxor"), vector [a, b]), PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_LSHIFT_unchecked (w1, w2) =>
                         (case (#targetLuaVersion ctx, w1, w2) of
                              (LUA5_3, Primitives.WORD, Primitives.WORD) => doBinaryOp (L.LSHIFT, PURE)
                            | (LUAJIT, Primitives.W32, Primitives.W32) => doBinaryExp (fn (a, b) => L.BinExp (L.MOD, L.CallExp (L.VarExp (L.PredefinedId "bit_lshift"), vector [a, b]), L.ConstExp (L.Numeral "0x100000000")), PURE)
                            | (LUAJIT, Primitives.W64, Primitives.W32) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "bit_lshift"), vector [a, b]), PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Word_RSHIFT_unchecked (w1, w2) =>
                         (case (#targetLuaVersion ctx, w1, w2) of
                              (LUA5_3, Primitives.WORD, Primitives.WORD) => doBinaryOp (L.RSHIFT, PURE)
                            | (LUAJIT, Primitives.W32, Primitives.W32) => doBinaryExp (fn (a, b) => L.BinExp (L.MOD, L.CallExp (L.VarExp (L.PredefinedId "bit_rshift"), vector [a, b]), L.ConstExp (L.Numeral "0x100000000")), PURE)
                            | (LUAJIT, Primitives.W64, Primitives.W32) => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "bit_rshift"), vector [a, b]), PURE)
                            | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on this target")
                         )
                       | Primitives.Real_PLUS => doBinaryOp (L.PLUS, PURE)
                       | Primitives.Real_MINUS => doBinaryOp (L.MINUS, PURE)
                       | Primitives.Real_TIMES => (case #targetLuaVersion ctx of
                                                       LUA5_3 => doBinaryOp (L.TIMES, PURE)
                                                     | LUAJIT => doBinaryExp (fn (a, b) => L.CallExp (L.VarExp (L.PredefinedId "_Real_mul"), vector [a, b]), PURE)
                                                  )
                       | Primitives.Real_DIVIDE => doBinaryOp (L.DIV, PURE)
                       | Primitives.Real_TILDE => doUnaryExp ( fn a => case #targetLuaVersion ctx of
                                                                           LUA5_3 => L.UnaryExp (L.NEGATE, a)
                                                                         | LUAJIT => L.BinExp (L.MINUS, L.VarExp (L.PredefinedId "NEGATIVE_ZERO"), a)
                                                             , PURE
                                                             )
                       | Primitives.Real_LT => doBinaryOp (L.LT, PURE)
                       | Primitives.Real_GT => doBinaryOp (L.GT, PURE)
                       | Primitives.Real_LE => doBinaryOp (L.LE, PURE)
                       | Primitives.Real_GE => doBinaryOp (L.GE, PURE)
                       | Primitives.Char_EQUAL => doBinaryOp (L.EQUAL, PURE)
                       | Primitives.Char_LT => doBinaryOp (L.LT, PURE)
                       | Primitives.Char_GT => doBinaryOp (L.GT, PURE)
                       | Primitives.Char_LE => doBinaryOp (L.LE, PURE)
                       | Primitives.Char_GE => doBinaryOp (L.GE, PURE)
                       | Primitives.String_EQUAL => doBinaryOp (L.EQUAL, PURE)
                       | Primitives.String_LT => doBinaryOp (L.LT, PURE)
                       | Primitives.String_GT => doBinaryOp (L.GT, PURE)
                       | Primitives.String_LE => doBinaryOp (L.LE, PURE)
                       | Primitives.String_GE => doBinaryOp (L.GE, PURE)
                       | Primitives.String_HAT => doBinaryOp (L.CONCAT, PURE)
                       | Primitives.String_size _ => doUnaryExp (fn a => L.UnaryExp (L.LENGTH, a), PURE)
                       | Primitives.String_str => doUnaryExp (fn a => L.CallExp (L.VarExp (L.PredefinedId "string_char"), vector [a]), PURE)
                       | Primitives.Vector_length _ => doUnaryExp (fn a => L.IndexExp (a, L.ConstExp (L.LiteralString "n")), PURE)
                       | Primitives.Vector_unsafeFromListRevN _ => doBinaryExp (fn (n, xs) => L.CallExp (L.VarExp (L.PredefinedId "_Vector_unsafeFromListRevN"), vector [n, xs]), PURE)
                       | Primitives.Array_EQUAL => doBinaryOp (L.EQUAL, PURE)
                       | Primitives.Array_length _ => doUnaryExp (fn a => L.IndexExp (a, L.ConstExp (L.LiteralString "n")), PURE)
                       | Primitives.Unsafe_Vector_sub _ => doBinaryExp (fn (vec, i) => L.IndexExp (vec, L.BinExp (L.PLUS, i, L.ConstExp (L.Numeral "1"))), PURE)
                       | Primitives.Unsafe_Array_sub _ => doBinaryExp (fn (arr, i) => L.IndexExp (arr, L.BinExp (L.PLUS, i, L.ConstExp (L.Numeral "1"))), IMPURE)
                       | Primitives.Unsafe_Array_update _ => doTernary (fn (arr, i, v) =>
                                                                           action (result, L.AssignStat ([L.IndexExp (arr, L.BinExp (L.PLUS, i, L.ConstExp (L.Numeral "1")))], [v]))
                                                                       )
                       | Primitives.Exception_instanceof => doBinaryExp (fn (e, tag) => L.CallExp (L.VarExp (L.PredefinedId "__exn_instanceof"), vector [e, tag]), PURE)
                       | Primitives.Lua_sub => doBinaryExp (fn (a, b) => L.IndexExp (a, b), IMPURE)
                       | Primitives.Lua_set => doTernary (fn (a, b, c) =>
                                                             action (result, L.AssignStat ([L.IndexExp (a, b)], [c]))
                                                         )
                       | Primitives.Lua_isNil => doUnaryExp (fn a => L.BinExp (L.EQUAL, a, L.ConstExp L.Nil), PURE)
                       | Primitives.Lua_EQUAL => doBinaryOp (L.EQUAL, IMPURE)
                       | Primitives.Lua_NOTEQUAL => doBinaryOp (L.NOTEQUAL, IMPURE)
                       | Primitives.Lua_LT => doBinaryOp (L.LT, IMPURE)
                       | Primitives.Lua_GT => doBinaryOp (L.GT, IMPURE)
                       | Primitives.Lua_LE => doBinaryOp (L.LE, IMPURE)
                       | Primitives.Lua_GE => doBinaryOp (L.GE, IMPURE)
                       | Primitives.Lua_PLUS => doBinaryOp (L.PLUS, IMPURE)
                       | Primitives.Lua_MINUS => doBinaryOp (L.MINUS, IMPURE)
                       | Primitives.Lua_TIMES => doBinaryOp (L.TIMES, IMPURE)
                       | Primitives.Lua_DIVIDE => doBinaryOp (L.DIV, IMPURE)
                       | Primitives.Lua_INTDIV => doBinaryOp (L.INTDIV, IMPURE)
                       | Primitives.Lua_MOD => doBinaryOp (L.MOD, IMPURE)
                       | Primitives.Lua_pow => doBinaryOp (L.POW, IMPURE)
                       | Primitives.Lua_unm => doUnaryExp (fn a => L.UnaryExp (L.NEGATE, a), IMPURE)
                       | Primitives.Lua_andb => doBinaryOp (L.BITAND, IMPURE) (* not used on LuaJIT *)
                       | Primitives.Lua_orb => doBinaryOp (L.BITOR, IMPURE) (* not used on LuaJIT *)
                       | Primitives.Lua_xorb => doBinaryOp (L.BITXOR, IMPURE) (* not used on LuaJIT *)
                       | Primitives.Lua_notb => doUnaryExp (fn a => L.UnaryExp (L.BITNOT, a), IMPURE) (* not used on LuaJIT *)
                       | Primitives.Lua_LSHIFT => doBinaryOp (L.LSHIFT, IMPURE) (* not used on LuaJIT *)
                       | Primitives.Lua_RSHIFT => doBinaryOp (L.RSHIFT, IMPURE) (* not used on LuaJIT *)
                       | Primitives.Lua_concat => doBinaryOp (L.CONCAT, IMPURE)
                       | Primitives.Lua_length => doUnaryExp (fn a => L.UnaryExp (L.LENGTH, a), IMPURE)
                       | Primitives.Lua_isFalsy => doUnaryExp (fn a => L.UnaryExp (L.NOT, a), PURE)
                       | Primitives.Lua_call => doBinary (fn (f, args) =>
                                                             let val arg = vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [args, L.ConstExp (L.Numeral "1"), L.IndexExp (args, L.ConstExp (L.LiteralString "n"))])]
                                                             in case result of
                                                                    SOME result => doDecs (ctx, env, decs, finalExp, L.ConstStat (result, L.CallExp (L.VarExp (L.PredefinedId "table_pack"), vector [L.CallExp (f, arg)])) :: revStats)
                                                                  | NONE => doDecs (ctx, env, decs, finalExp, L.CallStat (f, arg) :: revStats)
                                                             end
                                                         )
                       | Primitives.Lua_call1 => doBinary (fn (f, args) =>
                                                              let val arg = vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [args, L.ConstExp (L.Numeral "1"), L.IndexExp (args, L.ConstExp (L.LiteralString "n"))])]
                                                              in impure (result, L.CallExp (f, arg))
                                                              end
                                                          )
                       | Primitives.Lua_call2 => doBinary (fn (f, args) =>
                                                              let val arg = vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [args, L.ConstExp (L.Numeral "1"), L.IndexExp (args, L.ConstExp (L.LiteralString "n"))])]
                                                                  val stmts = case result of
                                                                                  NONE => [L.CallStat (f, arg)]
                                                                                | SOME result => let val r0 = genSym ctx
                                                                                                     val r1 = genSym ctx
                                                                                                 in [ L.LocalStat ([(r0, L.CONST), (r1, L.CONST)], [L.CallExp (f, arg)])
                                                                                                    , L.ConstStat (result, L.TableExp (vector [(L.IntKey 1, L.VarExp (L.UserDefinedId r0)), (L.IntKey 2, L.VarExp (L.UserDefinedId r1))]))
                                                                                                    ]
                                                                                                 end
                                                              in doDecs (ctx, env, decs, finalExp, List.revAppend (stmts, revStats))
                                                              end
                                                          )
                       | Primitives.Lua_call3 => doBinary (fn (f, args) =>
                                                              let val arg = vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [args, L.ConstExp (L.Numeral "1"), L.IndexExp (args, L.ConstExp (L.LiteralString "n"))])]
                                                                  val stmts = case result of
                                                                                  NONE => [L.CallStat (f, arg)]
                                                                                | SOME result => let val r0 = genSym ctx
                                                                                                     val r1 = genSym ctx
                                                                                                     val r2 = genSym ctx
                                                                                                 in [ L.LocalStat ([(r0, L.CONST), (r1, L.CONST), (r2, L.CONST)], [L.CallExp (f, arg)])
                                                                                                    , L.ConstStat (result, L.TableExp (vector [(L.IntKey 1, L.VarExp (L.UserDefinedId r0)), (L.IntKey 2, L.VarExp (L.UserDefinedId r1)), (L.IntKey 3, L.VarExp (L.UserDefinedId r2))]))
                                                                                                    ]
                                                                                                 end
                                                              in doDecs (ctx, env, decs, finalExp, List.revAppend (stmts, revStats))
                                                              end
                                                          )
                       | Primitives.Lua_method => doTernary (fn (obj, rawName, args) =>
                                                                let val name' = case rawName of
                                                                                    L.ConstExp (L.LiteralString name) =>
                                                                                    if LuaWriter.isLuaIdentifier name then
                                                                                        SOME name
                                                                                    else
                                                                                        NONE
                                                                                  | _ => NONE
                                                                in case name' of
                                                                       SOME name => let val arg = vector [L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [args, L.ConstExp (L.Numeral "1"), L.IndexExp (args, L.ConstExp (L.LiteralString "n"))])]
                                                                                    in case result of
                                                                                           SOME result => doDecs (ctx, env, decs, finalExp, L.ConstStat (result, L.CallExp (L.VarExp (L.PredefinedId "table_pack"), vector [L.MethodExp (obj, name, arg)])) :: revStats)
                                                                                         | NONE => doDecs (ctx, env, decs, finalExp, L.MethodStat (obj, name, arg) :: revStats)
                                                                                    end
                                                                     | NONE => let val arg = vector [obj, L.CallExp (L.VarExp (L.PredefinedId "table_unpack"), vector [args, L.ConstExp (L.Numeral "1"), L.IndexExp (args, L.ConstExp (L.LiteralString "n"))])]
                                                                               in case result of
                                                                                      SOME result => doDecs (ctx, env, decs, finalExp, L.ConstStat (result, L.CallExp (L.VarExp (L.PredefinedId "table_pack"), vector [L.CallExp (L.IndexExp (obj, rawName), arg)])) :: revStats)
                                                                                    | NONE => doDecs (ctx, env, decs, finalExp, L.CallStat (L.IndexExp (obj, rawName), arg) :: revStats)
                                                                               end
                                                                end
                                                            )
                       | Primitives.Lua_global => doUnaryExp ( fn x => case #targetLuaVersion ctx of
                                                                           LUA5_3 => L.IndexExp (L.VarExp (L.PredefinedId "_ENV"), x)
                                                                         | LUAJIT => L.IndexExp (L.VarExp (L.PredefinedId "_G"), x)
                                                             , DISCARDABLE
                                                             )
                       | Primitives.Lua_setGlobal => doBinary (fn (name, value) =>
                                                                  let val t = case #targetLuaVersion ctx of
                                                                                  LUA5_3 => L.IndexExp (L.VarExp (L.PredefinedId "_ENV"), name)
                                                                                | LUAJIT => L.IndexExp (L.VarExp (L.PredefinedId "_G"), name)
                                                                      val stmt = L.AssignStat ([t], [value])
                                                                  in action (result, stmt)
                                                                  end
                                                              )
                       | Primitives.Lua_newTable => discardable (result, L.TableExp (vector []))
                       | Primitives.DelimCont_newPromptTag => discardable (result, L.TableExp (vector []))
                       | Primitives.assumeDiscardable => doBinaryExp (fn (f, arg) => L.CallExp (f, vector [arg]), IMPURE)
                       | _ => raise CodeGenError ("primop " ^ Primitives.toString prim  ^ " is not supported on Lua backend")
                  end
                | C.ValDec { exp = C.PrimOp { primOp = F.LuaCallOp, tyargs = _, args = f :: args }, result } =>
                  (case result of
                       NONE => doDecs (ctx, env, decs, finalExp, L.CallStat (doValue ctx f, Vector.map (doValue ctx) (vector args)) :: revStats)
                     | SOME result => doDecs (ctx, env, decs, finalExp, L.ConstStat (result, L.CallExp (L.VarExp (L.PredefinedId "table_pack"), vector [L.CallExp (doValue ctx f, Vector.map (doValue ctx) (vector args))])) :: revStats)
                  )
                | C.ValDec { exp = C.PrimOp { primOp = F.LuaCall1Op, tyargs = _, args = f :: args }, result } =>
                  (case result of
                       NONE => doDecs (ctx, env, decs, finalExp, L.CallStat (doValue ctx f, Vector.map (doValue ctx) (vector args)) :: revStats)
                     | SOME result => doDecs (ctx, env, decs, finalExp, L.ConstStat (result, L.CallExp (doValue ctx f, Vector.map (doValue ctx) (vector args))) :: revStats)
                  )
                | C.ValDec { exp = C.PrimOp { primOp = F.LuaMethodOp name, tyargs = _, args = obj :: args }, result } =>
                  (case result of
                       NONE => doDecs (ctx, env, decs, finalExp, L.MethodStat (doValue ctx obj, name, Vector.map (doValue ctx) (vector args)) :: revStats)
                     | SOME result => doDecs (ctx, env, decs, finalExp, L.ConstStat (result, L.CallExp (L.VarExp (L.PredefinedId "table_pack"), vector [L.MethodExp (doValue ctx obj, name, Vector.map (doValue ctx) (vector args))])) :: revStats)
                  )
                | C.ValDec { exp = C.PrimOp { primOp = F.JsCallOp, tyargs = _, args = _ }, result } =>
                  raise CodeGenError "JsCallOp is not supported on Lua backend"
                | C.ValDec { exp = C.PrimOp { primOp = F.JsMethodOp, tyargs = _, args = _ }, result } =>
                  raise CodeGenError "JsMethodOp is not supported on Lua backend"
                | C.ValDec { exp = C.PrimOp { primOp = F.JsNewOp, tyargs = _, args = _ }, result } =>
                  raise CodeGenError "JsNewOp is not supported on Lua backend"
                | C.ValDec { exp = C.PrimOp { primOp, tyargs = _, args = _ }, result } =>
                  raise CodeGenError ("primop " ^ Printer.build (FPrinter.doPrimOp primOp) ^ " not implemented yet")
                | C.ValDec { exp = C.Record fields, result } => (* non-empty record *)
                  let val fields = Syntax.LabelMap.foldri (fn (label, v, acc) => (LabelToTableKey label, doValue ctx v) :: acc) [] fields
                  in pure (result, L.TableExp (vector fields))
                  end
                | C.ValDec { exp = C.ExnTag { name, payloadTy }, result } =>
                  discardable (result, L.TableExp (vector [(L.IntKey 1, L.ConstExp (L.LiteralString name))]))
                | C.ValDec { exp = C.Projection { label, record, fieldTypes }, result } =>
                  let val label = case label of
                                      Syntax.NumericLabel n => L.ConstExp (L.Numeral (Int.toString n))
                                    | Syntax.IdentifierLabel s => L.ConstExp (L.LiteralString s)
                  in pure (result, L.IndexExp (doValue ctx record, label))
                  end
                | C.ValDec { exp = C.Abs { contParam, params, body }, result } =>
                  let val env' = { continuations = C.CVarMap.singleton (contParam, RETURN) }
                  in pure (result, L.FunctionExp (Vector.map (fn vid => VIdToLua (ctx, vid)) (vector params), vector (doCExp ctx env' body)))
                  end
                | C.RecDec defs =>
                  let val (decs', assignments) = List.foldr (fn ((name, contParam, params, body), (decs, assignments)) =>
                                                                let val env' = { continuations = C.CVarMap.singleton (contParam, RETURN) }
                                                                    val dec = (name, L.LATE_INIT)
                                                                    val assignment = L.AssignStat ([L.VarExp (VIdToLua (ctx, name))], [L.FunctionExp (Vector.map (fn vid => VIdToLua (ctx, vid)) (vector params), vector (doCExp ctx env' body))])
                                                                in (dec :: decs, assignment :: assignments)
                                                                end
                                                            ) ([], []) defs
                  in doDecs (ctx, env, decs, finalExp, List.revAppend (assignments, L.LocalStat (decs', []) :: revStats))
                  end
                | C.ContDec { name, params, body } =>
                  (case (VectorSlice.isEmpty decs, finalExp) of
                       (true, C.App { applied, cont, args }) => if cont = name then
                                                                    List.revAppend (revStats, L.LocalStat (List.map (fn p => (p, L.CONST)) params, [L.CallExp (doValue ctx applied, Vector.map (doValue ctx) (vector args))])
                                                                                              :: doCExp ctx env body)
                                                                else
                                                                    List.revAppend (revStats, doCExp ctx env finalExp) (* dead continuation elimination *)
                     | _ => let val label = doLabel name
                                val env' = { continuations = C.CVarMap.insert (#continuations env, name, GOTO { label = label, params = List.map (fn p => VIdToLua (ctx, p)) params })
                                           }
                                val decs' = if List.null params then
                                                []
                                            else
                                                [L.LocalStat (List.map (fn p => (p, L.LATE_INIT)) params, [])]
                            in List.revAppend (revStats, decs' @ L.makeDoStat (doDecs (ctx, env', decs, finalExp, [])) @ L.LabelStat label :: doCExp ctx env body) (* enclose with do statement? *)
                            end
                  )
                | C.RecContDec defs =>
                  let val maxParams = List.foldl (fn ((_, params, _), n) => Int.max (n, List.length params)) 0 defs
                      val commonParams = List.tabulate (maxParams, fn _ => genSym ctx)
                      val decs' = if maxParams > 0 then
                                      [L.LocalStat (List.map (fn v => (v, L.MUTABLE)) commonParams, [])]
                                  else
                                      []
                      val env' = { continuations = List.foldl (fn ((name, params, body), m) => C.CVarMap.insert (m, name, GOTO { label = doLabel name, params = List.map L.UserDefinedId (List.take (commonParams, List.length params)) })) (#continuations env) defs
                                 }
                      val conts = List.map (fn (name, params, body) =>
                                               let val dec = if List.null params then
                                                                 []
                                                             else
                                                                 [L.LocalStat (List.map (fn v => (v, L.CONST)) params, List.map (fn v => L.VarExp (L.UserDefinedId v)) (List.take (commonParams, List.length params)))]
                                               in L.LabelStat (doLabel name) :: L.makeDoStat (dec @ doCExp ctx env' body)
                                               end
                                           ) defs
                  in List.revAppend (revStats, decs' @ L.makeDoStat (doDecs (ctx, env', decs, finalExp, [])) @ List.concat conts)
                  end
           end
      )
and doCExp (ctx : Context) (env : Env) (C.Let { decs, cont })
    = doDecs (ctx, env, VectorSlice.full decs, cont, [])
  | doCExp ctx env (C.App { applied, cont, args })
    = (case C.CVarMap.find (#continuations env, cont) of
           SOME (GOTO { label, params }) =>
           let val callAndAssign = case params of
                                       [] => L.CallStat (doValue ctx applied, Vector.map (doValue ctx) (vector args))
                                     | _ => L.AssignStat (List.map L.VarExp params, [L.CallExp (doValue ctx applied, Vector.map (doValue ctx) (vector args))])
           in [callAndAssign, L.GotoStat label]
           end
         | SOME RETURN => [L.ReturnStat (vector [L.CallExp (doValue ctx applied, Vector.map (doValue ctx) (vector args))])] (* tail call *)
         | NONE => raise CodeGenError "undefined continuation"
      )
  | doCExp ctx env (C.AppCont { applied, args }) = applyCont (ctx, env, applied, List.map (doValue ctx) args)
  | doCExp ctx env (C.If { cond, thenCont, elseCont })
    = let val thenLabel = L.UserDefinedId (genSymWithName (ctx, "then"))
          val elseLabel = L.UserDefinedId (genSymWithName (ctx, "else"))
      in if C.containsApp thenCont then
             L.IfStat (doValue ctx cond, vector [L.GotoStat thenLabel], vector [L.GotoStat elseLabel]) :: L.LabelStat thenLabel :: L.makeDoStat (doCExp ctx env thenCont) @ L.LabelStat elseLabel :: doCExp ctx env elseCont
         else
             L.IfStat (doValue ctx cond, vector (doCExp ctx env thenCont), vector []) :: doCExp ctx env elseCont (* ad hoc *)
      end
  | doCExp ctx env (C.Handle { body, handler = (e, h), successfulExitIn, successfulExitOut })
    = let val env' = { continuations = C.CVarMap.singleton (successfulExitIn, RETURN) }
          val status = genSymWithName (ctx, "status")
          val resultOrError = e
          val functionExp = L.FunctionExp (vector [], vector (doCExp ctx env' body))
      in [ L.LocalStat ([(status, L.CONST), (resultOrError, L.CONST)], [L.CallExp (L.VarExp (L.PredefinedId "_handle"), vector [functionExp])])
         , L.IfStat ( L.UnaryExp (L.NOT, L.VarExp (L.UserDefinedId status))
                    , vector (doCExp ctx env h)
                    , vector (applyCont (ctx, env, successfulExitOut, [L.VarExp (L.UserDefinedId e)]))
                    )
         ]
      end

fun doProgram ctx cont cexp
    = let val env = { continuations = C.CVarMap.singleton (cont, RETURN) }
      in vector (doCExp ctx env cexp)
      end
fun doProgramWithContinuations ctx cont cexp
    = let val env = { continuations = C.CVarMap.singleton (cont, RETURN) }
          val func = L.FunctionExp (vector [], vector (doCExp ctx env cexp))
      in vector [L.ReturnStat (vector [L.CallExp (L.VarExp (L.PredefinedId "_run"), vector [func])])]
      end
end;
