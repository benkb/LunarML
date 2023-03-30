(*
 * Copyright (c) 2021 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure DesugarPatternMatches = struct
exception DesugarError of SourcePos.span list * string
structure F = FSyntax
type Context = { nextVId : int ref
               , nextTyVar : int ref
               , targetInfo : TargetInfo.target_info
               }
fun freshVId(ctx : Context, name: string) = let val n = !(#nextVId ctx)
                                            in #nextVId ctx := n + 1
                                             ; TypedSyntax.MkVId (name, n)
                                            end
(* Check if the pattern is exhaustive and binds no variable *)
fun isWildcardPat (F.WildcardPat _) = true
  | isWildcardPat (F.SConPat _) = false
  | isWildcardPat (F.VarPat _) = false
  | isWildcardPat (F.RecordPat { sourceSpan = _, fields, ellipsis = NONE }) = List.all (fn (label, pat) => isWildcardPat pat) fields
  | isWildcardPat (F.RecordPat { sourceSpan = _, fields, ellipsis = SOME basePat }) = isWildcardPat basePat andalso List.all (fn (label, pat) => isWildcardPat pat) fields
  | isWildcardPat (F.ValConPat _) = false (* TODO *)
  | isWildcardPat (F.ExnConPat _) = false
  | isWildcardPat (F.LayeredPat _) = false
  | isWildcardPat (F.VectorPat (_, pats, ellipsis, _)) = ellipsis andalso Vector.length pats = 0
fun desugarPatternMatches (ctx: Context): { doExp: F.Exp -> F.Exp, doDec : F.Dec -> F.Dec, doDecs : F.Dec list -> F.Dec list }
    = let fun doExp exp0
              = (case exp0 of
                     F.PrimExp (primOp, tyargs, args) => F.PrimExp (primOp, tyargs, List.map doExp args)
                   | F.VarExp longvid => exp0
                   | F.RecordExp fields => F.RecordExp (List.map (fn (label, e) => (label, doExp e)) fields)
                   | F.LetExp (dec, exp) => F.LetExp (doDec dec, doExp exp)
                   | F.AppExp (exp1, exp2) => F.AppExp (doExp exp1, doExp exp2)
                   | F.HandleExp { body, exnName, handler } => F.HandleExp { body = doExp body, exnName = exnName, handler = doExp handler }
                   | F.IfThenElseExp (exp1, exp2, exp3) => F.IfThenElseExp (doExp exp1, doExp exp2, doExp exp3)
                   | F.FnExp (vid, ty, exp) => F.FnExp (vid, ty, doExp exp)
                   | F.ProjectionExp { label, record, fieldTypes } => F.ProjectionExp { label = label, record = doExp record, fieldTypes = fieldTypes }
                   | F.TyAbsExp (tv, kind, exp) => F.TyAbsExp (tv, kind, doExp exp)
                   | F.TyAppExp (exp, ty) => F.TyAppExp (doExp exp, ty)
                   | F.PackExp { payloadTy, exp, packageTy } => F.PackExp { payloadTy = payloadTy, exp = doExp exp, packageTy = packageTy }
                   | F.CaseExp (span, exp, ty, [(F.VarPat (span2, vid, ty'), exp2 as F.VarExp vid')]) =>
                     if TypedSyntax.eqVId (vid, vid') then
                         doExp exp
                     else
                         F.LetExp (F.ValDec (vid, SOME ty', doExp exp), exp2)
                   | F.CaseExp (span, exp, ty, matches) =>
                     let val canIgnore = case matches of
                                             [(pat, innerExp)] => if isWildcardPat pat then SOME innerExp else NONE
                                           | _ => NONE
                     in case canIgnore of
                            SOME innerExp => F.LetExp (F.IgnoreDec (doExp exp), doExp innerExp)
                          | NONE => let val examinedVId = freshVId(ctx, "exp")
                                        val examinedExp = F.VarExp(examinedVId)
                                        fun go [] = F.RaiseExp(span, (* TODO: type of raise *) F.RecordType Syntax.LabelMap.empty, F.VarExp(InitialEnv.VId_Match))
                                          | go ((pat, innerExp) :: rest)
                                            = let val binders = genBinders examinedExp ty pat
                                                  val matcher = genMatcher examinedExp ty pat
                                              in if isExhaustive pat then
                                                     if List.null rest then
                                                         List.foldr (fn (valbind, exp) => F.LetExp (F.ValDec valbind, exp)) (doExp innerExp) binders
                                                     else
                                                         raise Fail "A redundant pattern match found"
                                                 else
                                                     F.IfThenElseExp (matcher, List.foldr (fn (valbind, exp) => F.LetExp (F.ValDec valbind, exp)) (doExp innerExp) binders, go rest)
                                              end
                                    in F.LetExp (F.ValDec (examinedVId, SOME ty, doExp exp), go matches)
                                    end
                     end
                )
          and doDec (F.ValDec (vid, optTy, exp)) = F.ValDec (vid, optTy, doExp exp)
            | doDec (F.RecValDec valbinds) = F.RecValDec (List.map (fn (v, ty, exp) => (v, ty, doExp exp)) valbinds)
            | doDec (F.UnpackDec (tv, kind, vid, ty, exp)) = F.UnpackDec (tv, kind, vid, ty, doExp exp)
            | doDec (F.IgnoreDec exp) = F.IgnoreDec (doExp exp)
            | doDec (dec as F.DatatypeDec datbinds) = dec
            | doDec (dec as F.ExceptionDec { name, tagName, payloadTy }) = dec
            | doDec (F.ExportValue exp) = F.ExportValue (doExp exp)
            | doDec (F.ExportModule fields) = F.ExportModule (Vector.map (fn (label, exp) => (label, doExp exp)) fields)
            | doDec (F.GroupDec (v, decs)) = F.GroupDec (v, doDecs decs)
          and genMatcher exp _ (F.WildcardPat _) : F.Exp = F.VarExp InitialEnv.VId_true (* always match *)
            | genMatcher exp ty (F.SConPat { sourceSpan, scon, equality, cookedValue }) = F.AppExp (equality, F.TupleExp [exp, cookedValue])
            | genMatcher exp ty (F.VarPat (_, vid, _)) = F.VarExp InitialEnv.VId_true (* always match *)
            | genMatcher exp (recordTy as F.RecordType fieldTypes) (F.RecordPat { sourceSpan, fields, ellipsis = NONE })
              = List.foldr (fn ((label, pat), e) =>
                               case Syntax.LabelMap.find (fieldTypes, label) of
                                   SOME fieldTy => let val exp = genMatcher (F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }) fieldTy pat
                                                   in F.SimplifyingAndalsoExp (exp, e)
                                                   end
                                 | NONE => raise DesugarError ([sourceSpan], "internal error: record field not found (fieldTypes=" ^ FSyntax.PrettyPrint.print_Ty recordTy ^ ", " ^ Syntax.PrettyPrint.print_Label label ^ ")")
                           )
                           (F.VarExp InitialEnv.VId_true)
                           fields
            | genMatcher exp (recordTy as F.RecordType fieldTypes) (F.RecordPat { sourceSpan, fields, ellipsis = SOME basePat })
              = let val restTypes = List.foldl (fn ((label, _), fieldTypes) => #1 (Syntax.LabelMap.remove (fieldTypes, label))) fieldTypes fields
                    val restExp = F.RecordExp (Syntax.LabelMap.foldri (fn (label, fieldTy, xs) => (label, F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }) :: xs) [] restTypes)
                    val init = genMatcher restExp (F.RecordType restTypes) basePat
                in List.foldr (fn ((label, pat), e) =>
                                  case Syntax.LabelMap.find (fieldTypes, label) of
                                      SOME fieldTy => let val exp = genMatcher (F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }) fieldTy pat
                                                      in F.SimplifyingAndalsoExp (exp, e)
                                                      end
                                    | NONE => raise DesugarError ([sourceSpan], "internal error: record field not found (fieldTypes=" ^ FSyntax.PrettyPrint.print_Ty recordTy ^ ", " ^ Syntax.PrettyPrint.print_Label label ^ ")")
                              )
                              init
                              fields
                end
            | genMatcher exp _ (F.RecordPat { sourceSpan, fields, ellipsis }) = raise DesugarError ([sourceSpan], "internal error: record pattern against non-record type")
            | genMatcher exp ty (F.ValConPat { sourceSpan, info, payload = SOME (payloadTy, payloadPat) })
              = (case info of
                     { representation = Syntax.REP_LIST, tag = "::", ... } =>
                     let val elemTy = case ty of
                                          F.AppType { applied, arg } => arg
                                        | _ => raise DesugarError ([sourceSpan], "internal error: nil pattern with invalid type")
                         val hdExp = F.PrimExp (F.PrimFnOp Primitives.List_unsafeHead, [elemTy], [exp])
                         val tlExp = F.PrimExp (F.PrimFnOp Primitives.List_unsafeTail, [elemTy], [exp])
                         val payload = genMatcher (F.TupleExp [hdExp, tlExp]) payloadTy payloadPat
                     in F.SimplifyingAndalsoExp (F.PrimExp (F.PrimFnOp Primitives.Bool_not, [], [F.PrimExp (F.PrimFnOp Primitives.List_null, [elemTy], [exp])]), payload)
                     end
                   | { representation = Syntax.REP_REF, tag = "ref", ... } =>
                     genMatcher (F.PrimExp (F.PrimFnOp Primitives.Ref_read, [payloadTy], [exp])) payloadTy payloadPat
                   | { tag, ... } =>
                     let val payload = genMatcher (F.PrimExp (F.DataPayloadOp info, [payloadTy], [exp])) payloadTy payloadPat
                         val (dataTagOp, equalTag) = case #datatypeTag (#targetInfo ctx) of
                                                         TargetInfo.STRING8 => (F.DataTagAsStringOp, Primitives.String_EQUAL)
                                                       | TargetInfo.STRING16 => (F.DataTagAsString16Op, Primitives.String16_EQUAL)
                     in F.SimplifyingAndalsoExp (F.PrimExp (F.PrimFnOp equalTag, [], [F.PrimExp (dataTagOp info, [], [exp]), F.AsciiStringAsDatatypeTag (#targetInfo ctx, tag)]), payload)
                     end
                )
            | genMatcher exp ty (F.ValConPat { sourceSpan, info, payload = NONE })
              = (case info of
                     { representation = Syntax.REP_BOOL, tag = "true", ... } => exp
                   | { representation = Syntax.REP_BOOL, tag = "false", ... } => F.PrimExp (F.PrimFnOp Primitives.Bool_not, [], [exp])
                   | { representation = Syntax.REP_LIST, tag = "nil", ... } =>
                     let val elemTy = case ty of
                                          F.AppType { applied, arg } => arg
                                        | _ => raise DesugarError ([sourceSpan], "internal error: nil pattern with invalid type")
                     in F.PrimExp (F.PrimFnOp Primitives.List_null, [elemTy], [exp])
                     end
                   | { representation = _, tag, ... } =>
                     let val (dataTagOp, equalTag) = case #datatypeTag (#targetInfo ctx) of
                                                         TargetInfo.STRING8 => (F.DataTagAsStringOp, Primitives.String_EQUAL)
                                                       | TargetInfo.STRING16 => (F.DataTagAsString16Op, Primitives.String16_EQUAL)
                     in F.PrimExp (F.PrimFnOp equalTag, [], [F.PrimExp (dataTagOp info, [], [exp]), F.AsciiStringAsDatatypeTag (#targetInfo ctx, tag)])
                     end
                )
            | genMatcher exp ty (F.ExnConPat { sourceSpan = _, tagPath = tag, payload = SOME (payloadTy, payloadPat) })
              = let val payload = genMatcher (F.PrimExp (F.ExnPayloadOp, [payloadTy], [exp])) payloadTy payloadPat
                in F.SimplifyingAndalsoExp (F.PrimExp (F.PrimFnOp Primitives.Exception_instanceof, [], [exp, tag]), payload)
                end
            | genMatcher exp ty (F.ExnConPat { sourceSpan = _, tagPath = tag, payload = NONE })
              = F.PrimExp (F.PrimFnOp Primitives.Exception_instanceof, [], [exp, tag])
            | genMatcher exp ty0 (F.LayeredPat (span, vid, ty1, innerPat)) = genMatcher exp ty0 innerPat
            | genMatcher exp ty0 (F.VectorPat (span, pats, ellipsis, elemTy))
              = let val vectorLengthExp = F.PrimExp (F.PrimFnOp (Primitives.Vector_length Primitives.INT), [elemTy], [exp])
                    val intTy = F.TyCon ([], Typing.primTyName_int)
                    val expectedLengthExp = F.IntConstExp (Int.toLarge (Vector.length pats), intTy)
                    val e0 = if ellipsis then
                                 F.PrimExp (F.PrimFnOp (Primitives.Int_GE Primitives.INT), [], [vectorLengthExp, expectedLengthExp])
                             else
                                 F.PrimExp (F.PrimFnOp (Primitives.Int_EQUAL Primitives.INT), [], [vectorLengthExp, expectedLengthExp])
                in Vector.foldri (fn (i, pat, e) => let val exp = genMatcher (F.PrimExp (F.PrimFnOp (Primitives.Unsafe_Vector_sub Primitives.INT), [elemTy], [exp, F.IntConstExp (Int.toLarge i, intTy)])) elemTy pat
                                                    in F.SimplifyingAndalsoExp (e, exp)
                                                    end
                                 ) e0 pats
                end
          and genBinders exp ty (F.WildcardPat _) = []
            | genBinders exp ty (F.SConPat _) = []
            | genBinders exp _ (F.VarPat (span, vid, ty)) = [(vid, SOME ty, exp)]
            | genBinders exp (F.RecordType fieldTypes) (F.RecordPat { sourceSpan, fields, ellipsis = NONE }) = List.concat (List.map (fn (label, innerPat) => genBinders (F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }) (Syntax.LabelMap.lookup (fieldTypes, label)) innerPat) fields)
            | genBinders exp (F.RecordType fieldTypes) (F.RecordPat { sourceSpan, fields, ellipsis = SOME basePat })
              = let val restTypes = List.foldl (fn ((label, _), fieldTypes) => #1 (Syntax.LabelMap.remove (fieldTypes, label))) fieldTypes fields
                    val restExp = F.RecordExp (Syntax.LabelMap.foldri (fn (label, fieldTy, xs) => (label, F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }) :: xs) [] restTypes)
                in genBinders restExp (F.RecordType restTypes) basePat @ List.concat (List.map (fn (label, innerPat) => genBinders (F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes }) (Syntax.LabelMap.lookup (fieldTypes, label)) innerPat) fields)
                end
            | genBinders exp _ (F.RecordPat { sourceSpan, fields, ellipsis }) = raise DesugarError ([sourceSpan], "internal error: record pattern against non-record type")
            | genBinders exp ty (F.ValConPat { sourceSpan, info, payload = SOME (payloadTy, payloadPat) })
              = (case info of
                     { representation = Syntax.REP_REF, tag = "ref", ... } => genBinders (F.PrimExp (F.PrimFnOp Primitives.Ref_read, [payloadTy], [exp])) payloadTy payloadPat
                   | { representation = Syntax.REP_LIST, tag = "::", ... } =>
                     let val elemTy = case ty of
                                          F.AppType { applied, arg } => arg
                                        | _ => raise DesugarError ([sourceSpan], "internal error: nil pattern with invalid type")
                         val hdExp = F.PrimExp (F.PrimFnOp Primitives.List_unsafeHead, [elemTy], [exp])
                         val tlExp = F.PrimExp (F.PrimFnOp Primitives.List_unsafeTail, [elemTy], [exp])
                     in genBinders (F.TupleExp [hdExp, tlExp]) payloadTy payloadPat
                     end
                   | _ => genBinders (F.PrimExp (F.DataPayloadOp info, [payloadTy], [exp])) payloadTy payloadPat
                )
            | genBinders exp ty (F.ValConPat { sourceSpan, info, payload = NONE }) = []
            | genBinders exp ty (F.ExnConPat { sourceSpan = _, tagPath, payload = SOME (payloadTy, payloadPat) }) = genBinders (F.PrimExp (F.ExnPayloadOp, [payloadTy], [exp])) payloadTy payloadPat
            | genBinders exp ty (F.ExnConPat { sourceSpan = _, tagPath, payload = NONE }) = []
            | genBinders exp _ (F.LayeredPat (span, vid, ty, pat)) = (vid, SOME ty, exp) :: genBinders exp ty pat
            | genBinders exp ty (F.VectorPat (span, pats, ellipsis, elemTy)) = let val intTy = F.TyCon ([], Typing.primTyName_int)
                                                                               in Vector.foldri (fn (i, pat, acc) => genBinders (F.PrimExp (F.PrimFnOp (Primitives.Unsafe_Vector_sub Primitives.INT), [elemTy], [exp, F.IntConstExp (Int.toLarge i, intTy)])) elemTy pat @ acc) [] pats
                                                                               end
          and isExhaustive (F.WildcardPat _) = true
            | isExhaustive (F.SConPat _) = false
            | isExhaustive (F.VarPat _) = true
            | isExhaustive (F.RecordPat { sourceSpan = _, fields, ellipsis = NONE }) = List.all (fn (_, e) => isExhaustive e) fields
            | isExhaustive (F.RecordPat { sourceSpan = _, fields, ellipsis = SOME basePat }) = isExhaustive basePat andalso List.all (fn (_, e) => isExhaustive e) fields
            | isExhaustive (F.ValConPat _) = false (* TODO *)
            | isExhaustive (F.ExnConPat _) = false
            | isExhaustive (F.LayeredPat (_, _, _, innerPat)) = isExhaustive innerPat
            | isExhaustive (F.VectorPat (_, pats, ellipsis, elemTy)) = ellipsis andalso Vector.length pats = 0
          and doDecs decs = List.map doDec decs
      in { doExp = doExp
         , doDec = doDec
         , doDecs = doDecs
         }
      end
end (* structure DesugarPatternMatches *)

structure DecomposeValRec = struct
structure F = FSyntax
type Context = {}
fun doExp (F.PrimExp (primOp, tyargs, args)) = F.PrimExp (primOp, tyargs, List.map doExp args)
  | doExp (exp as F.VarExp _) = exp
  | doExp (F.RecordExp fields) = F.RecordExp (List.map (fn (label, exp) => (label, doExp exp)) fields)
  | doExp (F.LetExp (dec, exp)) = let val decs = doDec dec
                                  in List.foldr F.LetExp (doExp exp) decs
                                  end
  | doExp (F.AppExp (exp1, exp2)) = F.AppExp (doExp exp1, doExp exp2)
  | doExp (F.HandleExp { body, exnName, handler }) = F.HandleExp { body = doExp body
                                                                 , exnName = exnName
                                                                 , handler = doExp handler
                                                                 }
  | doExp (F.IfThenElseExp (exp1, exp2, exp3)) = F.IfThenElseExp (doExp exp1, doExp exp2, doExp exp3)
  | doExp (F.CaseExp (span, exp, ty, matches)) = F.CaseExp (span, doExp exp, ty, List.map (fn (pat, exp) => (pat, doExp exp)) matches)
  | doExp (F.FnExp (vid, ty, exp)) = F.FnExp (vid, ty, doExp exp)
  | doExp (F.ProjectionExp { label, record, fieldTypes }) = F.ProjectionExp { label = label, record = doExp record, fieldTypes = fieldTypes }
  | doExp (F.TyAbsExp (tv, kind, exp)) = F.TyAbsExp (tv, kind, doExp exp)
  | doExp (F.TyAppExp (exp, ty)) = F.TyAppExp (doExp exp, ty)
  | doExp (F.PackExp { payloadTy, exp, packageTy }) = F.PackExp { payloadTy = payloadTy, exp = doExp exp, packageTy = packageTy }
and doDec (F.ValDec (vid, optTy, exp)) = [F.ValDec (vid, optTy, doExp exp)]
  | doDec (F.RecValDec valbinds)
    = let val bound = List.foldl (fn ((vid, ty, exp), set) => TypedSyntax.VIdSet.add (set, vid)) TypedSyntax.VIdSet.empty valbinds
          val map : (F.Ty * F.Exp * (* refs *) TypedSyntax.VIdSet.set * (* invref *) TypedSyntax.VId list ref * (* seen1 *) bool ref * (* seen2 *) bool ref) TypedSyntax.VIdMap.map
              = List.foldl (fn ((vid, ty, exp), map) => let val exp = doExp exp
                                                        in TypedSyntax.VIdMap.insert (map, vid, (ty, exp, TypedSyntax.VIdSet.intersection (F.freeVarsInExp (TypedSyntax.VIdSet.empty, exp) TypedSyntax.VIdSet.empty, bound), ref [], ref false, ref false))
                                                        end) TypedSyntax.VIdMap.empty valbinds
          fun dfs1 (from : TypedSyntax.VId option, vid) : TypedSyntax.VId list
              = let val (ty, exp, refs, invref, seen1, _) = TypedSyntax.VIdMap.lookup (map, vid)
                    val () = case from of
                                 SOME vid' => invref := vid' :: !invref
                               | NONE => ()
                in if !seen1 then
                       []
                   else
                       ( seen1 := true
                       ; TypedSyntax.VIdSet.foldl (fn (vid', acc) => acc @ dfs1 (SOME vid, vid')) [vid] refs
                       )
                end
          val list : TypedSyntax.VId list = TypedSyntax.VIdMap.foldli (fn (vid, _, acc) => acc @ dfs1 (NONE, vid)) [] map
          fun dfs2 vid : TypedSyntax.VIdSet.set
              = let val (ty, exp, refs, ref invrefs, _, seen2) = TypedSyntax.VIdMap.lookup (map, vid)
                in if !seen2 then
                       TypedSyntax.VIdSet.empty
                   else
                       ( seen2 := true
                       ; List.foldl (fn (vid', acc) => TypedSyntax.VIdSet.union (acc, dfs2 vid')) (TypedSyntax.VIdSet.singleton vid) invrefs
                       )
                end
          val sccs : TypedSyntax.VIdSet.set list = List.foldl (fn (vid, acc) => let val set = dfs2 vid
                                                                                in if TypedSyntax.VIdSet.isEmpty set then
                                                                                       acc
                                                                                   else
                                                                                       set :: acc
                                                                                end) [] list
      in List.foldr (fn (scc, decs) => let val dec = case TypedSyntax.VIdSet.listItems scc of
                                                         [vid] => let val (ty, exp, refs, _, _, _) = TypedSyntax.VIdMap.lookup (map, vid)
                                                                  in if TypedSyntax.VIdSet.member (refs, vid) then
                                                                         F.RecValDec [(vid, ty, exp)]
                                                                     else
                                                                         F.ValDec (vid, SOME ty, exp)
                                                                  end
                                                       | scc => F.RecValDec (List.foldl (fn (vid, xs) =>
                                                                                            let val (ty, exp, _, _, _, _) = TypedSyntax.VIdMap.lookup (map, vid)
                                                                                            in (vid, ty, exp) :: xs
                                                                                            end
                                                                                        ) [] scc)
                                       in dec :: decs
                                       end
                    ) [] sccs
      end
  | doDec (F.UnpackDec (tv, kind, vid, ty, exp)) = [F.UnpackDec (tv, kind, vid, ty, doExp exp)]
  | doDec (F.IgnoreDec exp) = [F.IgnoreDec (doExp exp)]
  | doDec (F.DatatypeDec datbinds) = [F.DatatypeDec datbinds]
  | doDec (F.ExceptionDec names) = [F.ExceptionDec names]
  | doDec (F.ExportValue exp) = [F.ExportValue (doExp exp)]
  | doDec (F.ExportModule fields) = [F.ExportModule (Vector.map (fn (label, exp) => (label, doExp exp)) fields)]
  | doDec (F.GroupDec (set, decs)) = [F.GroupDec (set, doDecs decs)]
and doDecs decs = List.foldr (fn (dec, rest) => doDec dec @ rest) [] decs
end

structure DeadCodeElimination = struct
structure F = FSyntax
fun isDiscardablePrimOp (F.IntConstOp _) = true
  | isDiscardablePrimOp (F.WordConstOp _) = true
  | isDiscardablePrimOp (F.RealConstOp _) = true
  | isDiscardablePrimOp (F.Char8ConstOp _) = true
  | isDiscardablePrimOp (F.Char16ConstOp _) = true
  | isDiscardablePrimOp (F.String8ConstOp _) = true
  | isDiscardablePrimOp (F.String16ConstOp _) = true
  | isDiscardablePrimOp (F.RaiseOp _) = false
  | isDiscardablePrimOp F.ListOp = true
  | isDiscardablePrimOp F.VectorOp = true
  | isDiscardablePrimOp (F.DataTagAsStringOp _) = true
  | isDiscardablePrimOp (F.DataTagAsString16Op _) = true
  | isDiscardablePrimOp (F.DataPayloadOp _) = true
  | isDiscardablePrimOp F.ExnPayloadOp = true
  | isDiscardablePrimOp (F.ConstructValOp _) = true
  | isDiscardablePrimOp (F.ConstructValWithPayloadOp _) = true
  | isDiscardablePrimOp F.ConstructExnOp = true
  | isDiscardablePrimOp F.ConstructExnWithPayloadOp = true
  | isDiscardablePrimOp (F.PrimFnOp p) = Primitives.isDiscardable p
  | isDiscardablePrimOp F.JsCallOp = false
  | isDiscardablePrimOp F.JsMethodOp = false
  | isDiscardablePrimOp F.JsNewOp = false
  | isDiscardablePrimOp F.LuaCallOp = false
  | isDiscardablePrimOp F.LuaCall1Op = false
  | isDiscardablePrimOp (F.LuaMethodOp _) = false
fun isDiscardable (F.PrimExp (primOp, tyargs, args)) = isDiscardablePrimOp primOp andalso List.all isDiscardable args
  | isDiscardable (F.VarExp _) = true
  | isDiscardable (F.RecordExp fields) = List.all (fn (label, exp) => isDiscardable exp) fields
  | isDiscardable (F.LetExp (dec, exp)) = false (* TODO *)
  | isDiscardable (F.AppExp (exp1, exp2)) = false (* TODO *)
  | isDiscardable (F.HandleExp { body, exnName, handler }) = false (* TODO *)
  | isDiscardable (F.IfThenElseExp (exp1, exp2, exp3)) = isDiscardable exp1 andalso isDiscardable exp2 andalso isDiscardable exp3
  | isDiscardable (F.CaseExp (span, exp, ty, matches)) = false (* TODO *)
  | isDiscardable (F.FnExp (vid, ty, exp)) = true
  | isDiscardable (F.ProjectionExp { label, record, fieldTypes }) = isDiscardable record
  | isDiscardable (F.TyAbsExp (tyvar, kind, exp)) = isDiscardable exp
  | isDiscardable (F.TyAppExp (exp, ty)) = isDiscardable exp
  | isDiscardable (F.PackExp { payloadTy, exp, packageTy }) = isDiscardable exp
(* doPat : F.Pat -> TypedSyntax.VIdSet.set -> (* constructors used *) TypedSyntax.VIdSet.set *)
fun doPat (F.WildcardPat _) acc = acc
  | doPat (F.SConPat { sourceSpan = _, scon = _, equality, cookedValue }) acc = #1 (doExp equality (#1 (doExp cookedValue acc)))
  | doPat (F.VarPat _) acc = acc
  | doPat (F.RecordPat { sourceSpan = _, fields, ellipsis }) acc = List.foldl (fn ((label, pat), acc) => doPat pat acc) (case ellipsis of NONE => acc | SOME basePat => doPat basePat acc) fields
  | doPat (F.ValConPat { sourceSpan = _, info, payload = NONE }) acc = acc
  | doPat (F.ValConPat { sourceSpan = _, info, payload = SOME (payloadTy, payloadPat) }) acc = doPat payloadPat acc
  | doPat (F.ExnConPat { sourceSpan = _, tagPath, payload = NONE }) acc = #1 (doExp tagPath acc)
  | doPat (F.ExnConPat { sourceSpan = _, tagPath, payload = SOME (payloadTy, payloadPat) }) acc = doPat payloadPat (#1 (doExp tagPath acc))
  | doPat (F.LayeredPat (_, vid, ty, innerPat)) acc = doPat innerPat acc
  | doPat (F.VectorPat (_, pats, ellipsis, elemTy)) acc = Vector.foldl (fn (pat, acc) => doPat pat acc) acc pats
(* doExp : F.Exp -> TypedSyntax.VIdSet.set -> TypedSyntax.VIdSet.set * F.Exp *)
and doExp (F.PrimExp (primOp, tyargs, args) : F.Exp) acc : TypedSyntax.VIdSet.set * F.Exp
    = let val (acc, args') = List.foldr (fn (x, (acc, xs)) => let val (acc, x) = doExp x acc in (acc, x :: xs) end) (acc, []) args
      in (acc, F.PrimExp (primOp, tyargs, args'))
      end
  | doExp (exp as F.VarExp vid) acc = (TypedSyntax.VIdSet.add (acc, vid), exp)
  | doExp (F.RecordExp fields) acc = let val (acc, fields) = List.foldr (fn ((label, exp), (acc, xs)) => let val (acc, exp) = doExp exp acc in (acc, (label, exp) :: xs) end) (acc, []) fields
                                     in (acc, F.RecordExp fields)
                                     end
  | doExp (F.LetExp (dec, exp)) acc = let val (used, exp) = doExp exp TypedSyntax.VIdSet.empty
                                          val (used', decs) = doDec (used, dec)
                                      in (TypedSyntax.VIdSet.union (acc, used'), List.foldr F.LetExp exp decs)
                                      end
  | doExp (F.AppExp (exp1, exp2)) acc = let val (used, exp1) = doExp exp1 acc
                                            val (used', exp2) = doExp exp2 used
                                        in (used', F.AppExp (exp1, exp2))
                                        end
  | doExp (F.HandleExp { body, exnName, handler }) acc = let val (used, handler) = doExp handler TypedSyntax.VIdSet.empty
                                                             val used = TypedSyntax.VIdSet.subtract (used, exnName)
                                                             val (used', body) = doExp body (TypedSyntax.VIdSet.union (acc, used))
                                                         in (used', F.HandleExp { body = body, exnName = exnName, handler = handler })
                                                         end
  | doExp (F.IfThenElseExp (exp1, exp2, exp3)) acc = let val (used1, exp1) = doExp exp1 acc
                                                         val (used2, exp2) = doExp exp2 used1
                                                         val (used3, exp3) = doExp exp3 used2
                                                     in (used3, F.IfThenElseExp (exp1, exp2, exp3))
                                                     end
  | doExp (F.CaseExp (span, exp, ty, matches)) acc = let val (used, exp) = doExp exp acc
                                                         val (used, matches) = List.foldr (fn ((pat, exp), (used, matches)) => let val (used', exp) = doExp exp acc
                                                                                                                               in (doPat pat used', (pat, exp) :: matches)
                                                                                                                               end)
                                                                                          (used, []) matches
                                                     in (used, F.CaseExp (span, exp, ty, matches))
                                                     end
  | doExp (F.FnExp (vid, ty, exp)) acc = let val (used, exp) = doExp exp acc
                                         in (used, F.FnExp (vid, ty, exp))
                                         end
  | doExp (F.ProjectionExp { label, record, fieldTypes }) acc = let val (used, exp) = doExp record acc
                                                                in (used, F.ProjectionExp { label = label, record = exp, fieldTypes = fieldTypes })
                                                                end
  | doExp (F.TyAbsExp (tyvar, kind, exp)) acc = let val (used, exp) = doExp exp acc
                                                in (used, F.TyAbsExp (tyvar, kind, exp))
                                                end
  | doExp (F.TyAppExp (exp, ty)) acc = let val (used, exp) = doExp exp acc
                                       in (used, F.TyAppExp (exp, ty))
                                       end
  | doExp (F.PackExp { payloadTy, exp, packageTy }) acc = let val (used, exp) = doExp exp acc
                                                          in (used, F.PackExp { payloadTy = payloadTy, exp = exp, packageTy = packageTy })
                                                          end
and doIgnoredExpAsExp exp acc = let val (used, exps) = doIgnoredExp exp acc
                                in (used, List.foldr (fn (e1, e2) => F.LetExp (F.IgnoreDec e1, e2)) (F.RecordExp []) exps)
                                end
(* doIgnoredExp : F.Exp -> TypedSyntax.VIdSet.set -> TypedSyntax.VIdSet.set * F.Exp list *)
and doIgnoredExp (exp as F.PrimExp (primOp, tyargs, args)) acc
    = if isDiscardablePrimOp primOp then
          List.foldr (fn (x, (acc, xs)) => let val (acc, ys) = doIgnoredExp x acc in (acc, ys @ xs) end) (acc, []) args
      else
          let val (used, exp) = doExp exp acc
          in (used, [exp])
          end
  | doIgnoredExp (F.VarExp _) acc = (acc, [])
  | doIgnoredExp (F.RecordExp fields) acc = List.foldr (fn ((label, exp), (acc, xs)) => let val (acc, ys) = doIgnoredExp exp acc in (acc, ys @ xs) end) (acc, []) fields
  | doIgnoredExp (F.LetExp (dec, exp)) acc = let val (used, exp) = doIgnoredExpAsExp exp TypedSyntax.VIdSet.empty
                                                 val (used, decs) = doDec (used, dec)
                                             in case List.foldr F.LetExp exp decs of
                                                    F.RecordExp [] => (TypedSyntax.VIdSet.union (acc, used), [])
                                                  | exp => (TypedSyntax.VIdSet.union (acc, used), [exp])
                                             end
  | doIgnoredExp (F.AppExp (exp1, exp2)) acc = let val (used1, exp1) = doExp exp1 acc
                                                   val (used2, exp2) = doExp exp2 used1
                                               in (used2, [F.AppExp (exp1, exp2)])
                                               end
  | doIgnoredExp (F.HandleExp { body, exnName, handler }) acc = let val (used2, handler) = doIgnoredExpAsExp handler TypedSyntax.VIdSet.empty
                                                                    val used2 = TypedSyntax.VIdSet.subtract (used2, exnName)
                                                                    val (used1, body) = doIgnoredExpAsExp body (TypedSyntax.VIdSet.union (acc, used2))
                                                                in case body of
                                                                       F.RecordExp [] => (used1, [])
                                                                     | _ => (used1, [F.HandleExp { body = body, exnName = exnName, handler = handler }])
                                                                end
  | doIgnoredExp (F.IfThenElseExp (exp1, exp2, exp3)) acc = let val (used2, exp2) = doIgnoredExpAsExp exp2 acc
                                                                val (used3, exp3) = doIgnoredExpAsExp exp3 used2
                                                            in case (exp2, exp3) of
                                                                   (F.RecordExp [], F.RecordExp []) => doIgnoredExp exp1 used3
                                                                 | (exp2, exp3) => let val (used1, exp1) = doExp exp1 used3
                                                                                   in (used1, [F.IfThenElseExp (exp1, exp2, exp3)])
                                                                                   end
                                                            end
  | doIgnoredExp (F.CaseExp (span, exp, ty, matches)) acc = let val (used, exp) = doExp exp acc
                                                            val (used, matches) = List.foldr (fn ((pat, exp), (used, matches)) => let val (used', exp) = doIgnoredExpAsExp exp used
                                                                                                                                      val used'' = doPat pat used'
                                                                                                                                  in (used'', (pat, exp) :: matches)
                                                                                                                                  end)
                                                                                             (used, []) matches
                                                            in (used, [F.CaseExp (span, exp, ty, matches)])
                                                            end
  | doIgnoredExp (F.FnExp _) acc = (acc, [])
  | doIgnoredExp (F.ProjectionExp { label, record, fieldTypes }) acc = doIgnoredExp record acc
  | doIgnoredExp (F.TyAbsExp (tyvar, kind, exp)) acc = let val (used, exp) = doIgnoredExpAsExp exp acc (* should be pure *)
                                                       in case exp of
                                                              F.RecordExp [] => (used, [])
                                                            | exp => (used, [F.TyAbsExp (tyvar, kind, exp)])
                                                       end
  | doIgnoredExp (F.TyAppExp (exp, ty)) acc = let val (used, exp) = doIgnoredExpAsExp exp acc
                                              in case exp of
                                                     F.RecordExp [] => (used, [])
                                                   | exp => (used, [F.TyAppExp (exp, ty)])
                                              end
  | doIgnoredExp (F.PackExp { payloadTy, exp, packageTy }) acc = let val (used, exp) = doIgnoredExpAsExp exp acc
                                                                 in case exp of
                                                                        F.RecordExp [] => (used, [])
                                                                      | exp => (used, [F.PackExp { payloadTy = payloadTy, exp = exp, packageTy = packageTy }])
                                                                 end
(* doDec : TypedSyntax.VIdSet.set * F.Dec -> TypedSyntax.VIdSet.set * F.Dec list *)
and doDec (used : TypedSyntax.VIdSet.set, F.ValDec (vid, optTy, exp)) : TypedSyntax.VIdSet.set * F.Dec list
    = if not (TypedSyntax.VIdSet.member (used, vid)) then
          if isDiscardable exp then
              (used, [])
          else
              let val (used', exps) = doIgnoredExp exp used
              in (used', List.map F.IgnoreDec exps)
              end
      else
          let val (used', exp') = doExp exp used
          in (used', [F.ValDec (vid, optTy, exp')])
          end
  | doDec (used, F.RecValDec valbinds)
    = let val bound = List.foldl (fn ((vid, _, _), acc) => TypedSyntax.VIdSet.add (acc, vid)) TypedSyntax.VIdSet.empty valbinds
      in if TypedSyntax.VIdSet.disjoint (used, bound) then
             (used, []) (* RHS should be fn _ => _, and therefore discardable *)
         else
             let val (used, valbinds) = List.foldr (fn ((vid, ty, exp), (used, valbinds)) => let val (used', exp) = doExp exp used
                                                                                             in (used', (vid, ty, exp) :: valbinds)
                                                                                             end
                                                   ) (used, []) valbinds
             in (used, [F.RecValDec valbinds])
             end
      end
  | doDec (used, F.UnpackDec (tv, kind, vid, ty, exp))
    = if not (TypedSyntax.VIdSet.member (used, vid)) then
          if isDiscardable exp then
              (used, [])
          else
              let val (used', exps) = doIgnoredExp exp used
              in (used', List.map F.IgnoreDec exps)
              end
      else
          let val (used', exp') = doExp exp used
          in (used', [F.UnpackDec (tv, kind, vid, ty, exp')])
          end
  | doDec (used, F.IgnoreDec exp) = let val (used', exps) = doIgnoredExp exp used
                                    in (used', List.map F.IgnoreDec exps)
                                    end
  | doDec (used, dec as F.DatatypeDec datbinds) = (used, [dec]) (* TODO *)
  | doDec (used, dec as F.ExceptionDec { name, tagName, payloadTy }) = if TypedSyntax.VIdSet.member (used, tagName) then
                                                                           (used, [dec])
                                                                       else
                                                                           (used, [])
  | doDec (used, F.ExportValue exp) = let val (used', exp) = doExp exp used
                                      in (used', [F.ExportValue exp])
                                      end
  | doDec (used, F.ExportModule fields) = let val (acc, fields') = Vector.foldr (fn ((label, exp), (acc, xs)) => let val (acc, exp) = doExp exp acc in (acc, (label, exp) :: xs) end) (used, []) fields
                                          in (acc, [F.ExportModule (Vector.fromList fields')])
                                          end
  | doDec (used, F.GroupDec (_, decs)) = let val (used', decs) = doDecs (used, decs)
                                         in (used', case decs of
                                                        [] => decs
                                                      | [_] => decs
                                                      | _ => let val defined = definedInDecs decs TypedSyntax.VIdSet.empty
                                                             in [F.GroupDec (SOME (TypedSyntax.VIdSet.intersection (used, defined)), decs)]
                                                             end
                                            )
                                         end
(* doDecs : TypedSyntax.VIdSet.set * F.Dec list -> TypedSyntax.VIdSet.set * F.Dec list *)
and doDecs (used, decs) = List.foldr (fn (dec, (used, decs)) => let val (used, dec) = doDec (used, dec)
                                                                in (used, dec @ decs)
                                                                end) (used, []) decs
and definedInDecs decs acc = List.foldl (fn (dec, s) => definedInDec dec s) acc decs
and definedInDec (F.ValDec (vid, _, _)) acc = TypedSyntax.VIdSet.add (acc, vid)
  | definedInDec (F.RecValDec valbinds) acc = List.foldl (fn ((vid, _, _), s) => TypedSyntax.VIdSet.add (s, vid)) acc valbinds
  | definedInDec (F.UnpackDec (tv, kind, vid, ty, exp)) acc = TypedSyntax.VIdSet.add (acc, vid)
  | definedInDec (F.IgnoreDec _) acc = acc
  | definedInDec (F.DatatypeDec datbinds) acc = List.foldl (fn (F.DatBind (tyvars, tycon, conbinds), s) => List.foldl (fn (F.ConBind (vid, _), s) => TypedSyntax.VIdSet.add (s, vid)) s conbinds) acc datbinds
  | definedInDec (F.ExceptionDec { name = _, tagName, payloadTy = _ }) acc = TypedSyntax.VIdSet.add (acc, tagName)
  | definedInDec (F.ExportValue _) acc = acc (* should not occur *)
  | definedInDec (F.ExportModule _) acc = acc (* should not occur *)
  | definedInDec (F.GroupDec(_, decs)) acc = definedInDecs decs acc (* should not occur *)
end; (* structure DeadCodeElimination *)
