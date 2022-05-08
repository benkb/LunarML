(*
 * Copyright (c) 2022 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure FSyntax = struct
type TyVar = TypedSyntax.TyVar
datatype SLabel = ValueLabel of Syntax.VId
                | StructLabel of Syntax.StrId
                | ExnTagLabel of Syntax.VId (* of constructor *)
datatype Path = Root of TypedSyntax.VId
              | Child of Path * SLabel
              | Field of Path * Syntax.Label (* record field *)
datatype Kind = TypeKind
              | ArrowKind of Kind * Kind
datatype Ty = TyVar of TyVar
            | RecordType of Ty Syntax.LabelMap.map
            | AppType of { applied : Ty, arg : Ty }
            | FnType of Ty * Ty
            | ForallType of TyVar * Kind * Ty
            | ExistsType of TyVar * Kind * Ty
            | TypeFn of TyVar * Kind * Ty (* type-level function *)
            | SigType of { valMap : (Ty * Syntax.IdStatus) Syntax.VIdMap.map
                         , strMap : Ty Syntax.StrIdMap.map
                         , exnTags : Syntax.VIdSet.set
                         }
datatype ConBind = ConBind of TypedSyntax.VId * Ty option
datatype DatBind = DatBind of TyVar list * TyVar * ConBind list
datatype PrimOp = IntConstOp of IntInf.int (* 1 type argument *)
                | WordConstOp of IntInf.int (* 1 type argument *)
                | RealConstOp of Numeric.float_notation (* 1 type argument *)
                | StringConstOp of int vector (* narrow / wide, 1 type argument *)
                | CharConstOp of int (* narrow / wide, 1 type argument *)
                | RaiseOp of SourcePos.span (* type argument: result type, value argument: the exception *)
                | ListOp (* type argument: element type, value arguments: the elements *)
                | VectorOp (* type argument: element type, value arguments: the elements *)
                | RecordEqualityOp (* value argument: the record of equalities *)
                | DataTagOp (* value argument: the data *)
                | DataPayloadOp (* value argument: the data *)
                | PrimFnOp of Primitives.PrimOp
                | ExnInstanceofOp (* type argument: none, value arguments: exception, exception tag *)
datatype Pat = WildcardPat of SourcePos.span
             | SConPat of { sourceSpan : SourcePos.span
                          , scon : Syntax.SCon
                          , equality : Exp
                          , cookedValue : Exp
                          }
             | VarPat of SourcePos.span * TypedSyntax.VId * Ty
             | RecordPat of { sourceSpan : SourcePos.span
                            , fields : (Syntax.Label * Pat) list
                            , ellipsis : Pat option
                            }
             | ConPat of SourcePos.span * Path * Pat option * Ty list
             | LayeredPat of SourcePos.span * TypedSyntax.VId * Ty * Pat
             | VectorPat of SourcePos.span * Pat vector * bool * Ty
     and Exp = PrimExp of PrimOp * Ty vector * Exp vector
             | VarExp of TypedSyntax.VId
             | RecordExp of (Syntax.Label * Exp) list
             | LetExp of Dec * Exp
             | AppExp of Exp * Exp
             | HandleExp of { body : Exp
                            , exnName : TypedSyntax.VId
                            , handler : Exp
                            }
             | IfThenElseExp of Exp * Exp * Exp
             | CaseExp of SourcePos.span * Exp * Ty * (Pat * Exp) list
             | FnExp of TypedSyntax.VId * Ty * Exp
             | ProjectionExp of { label : Syntax.Label, record : Exp }
             | TyAbsExp of TyVar * Kind * Exp
             | TyAppExp of Exp * Ty
             | StructExp of { valMap : Path Syntax.VIdMap.map
                            , strMap : Path Syntax.StrIdMap.map
                            , exnTagMap : Path Syntax.VIdMap.map
                            }
             | SProjectionExp of Exp * SLabel
             | PackExp of { payloadTy : Ty, exp : Exp, packageTy : Ty } (* packageTy must be ExistsType *)
     and Dec = ValDec of TypedSyntax.VId * Ty option * Exp
             | RecValDec of (TypedSyntax.VId * Ty * Exp) list
             | UnpackDec of TyVar * Kind * TypedSyntax.VId * (* the type of the new identifier *) Ty * Exp
             | IgnoreDec of Exp (* val _ = ... *)
             | DatatypeDec of DatBind list
             | ExceptionDec of { conName : TypedSyntax.VId, tagName : TypedSyntax.VId, payloadTy : Ty option }
             | ExceptionRepDec of { conName : TypedSyntax.VId, conPath : Path, tagPath : Path, payloadTy : Ty option }
             | ExportValue of Exp
             | ExportModule of (string * Exp) vector
             | GroupDec of TypedSyntax.VIdSet.set option * Dec list
fun IntConstExp (value, ty) = PrimExp (IntConstOp value, vector [ty], vector [])
fun WordConstExp (value, ty) = PrimExp (WordConstOp value, vector [ty], vector [])
fun RaiseExp (span, ty, exp) = PrimExp (RaiseOp span, vector [ty], vector [exp])
fun ListExp (exps, elemTy) = PrimExp (ListOp, vector [elemTy], exps)
fun VectorExp (exps, elemTy) = PrimExp (VectorOp, vector [elemTy], exps)
fun RecordEqualityExp fields = PrimExp (RecordEqualityOp, vector [], vector [RecordExp fields])
fun DataTagExp exp = PrimExp (DataTagOp, vector [], vector [exp])
fun DataPayloadExp exp = PrimExp (DataPayloadOp, vector [], vector [exp])
fun TupleType xs = let fun doFields i [] acc = acc
                         | doFields i (x :: xs) acc = doFields (i + 1) xs (Syntax.LabelMap.insert (acc, Syntax.NumericLabel i, x))
                   in RecordType (doFields 1 xs Syntax.LabelMap.empty)
                   end
fun PairType(a, b) = RecordType (Syntax.LabelMapFromList [(Syntax.NumericLabel 1, a), (Syntax.NumericLabel 2, b)])
fun TuplePat (span, xs) = let fun doFields i nil = nil
                                | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
                          in RecordPat { sourceSpan = span, fields = doFields 1 xs, ellipsis = NONE }
                          end
fun TupleExp xs = let fun doFields i nil = nil
                        | doFields i (x :: xs) = (Syntax.NumericLabel i, x) :: doFields (i + 1) xs
                  in RecordExp (doFields 1 xs)
                  end
fun tyNameToTyVar (TypedSyntax.MkTyName (name, n)) = TypedSyntax.NamedTyVar (name, false, n)
fun TyCon(tyargs, tyname) = List.foldl (fn (arg, applied) => AppType { applied = applied, arg = arg }) (TyVar (tyNameToTyVar tyname)) tyargs
fun AsciiStringAsNativeString (targetInfo : TargetInfo.target_info, s : string) = let val ty = case #nativeString targetInfo of
                                                                                                   TargetInfo.NARROW_STRING => TyCon ([], Typing.primTyName_string)
                                                                                                 | TargetInfo.WIDE_STRING => TyCon ([], Typing.primTyName_wideString)
                                                                                  in PrimExp (StringConstOp (StringElement.encodeAscii s), vector [ty], vector [])
                                                                                  end
fun strIdToVId (TypedSyntax.MkStrId (name, n)) = TypedSyntax.MkVId (name, n)
fun LongVarExp (TypedSyntax.MkShortVId vid) = VarExp vid
  | LongVarExp (TypedSyntax.MkLongVId (strid0, strids, vid)) = SProjectionExp (List.foldl (fn (label, x) => SProjectionExp (x, StructLabel label)) (VarExp (strIdToVId strid0)) strids, ValueLabel vid)
fun PathToExp(Root vid) = VarExp vid
  | PathToExp(Child (parent, label)) = SProjectionExp (PathToExp parent, label)
  | PathToExp(Field (parent, label)) = ProjectionExp { label = label, record = PathToExp parent }
fun rootOfPath(Root vid) = vid
  | rootOfPath(Child (parent, _)) = rootOfPath parent
  | rootOfPath(Field (parent, _)) = rootOfPath parent
fun AndalsoExp(a, b) = IfThenElseExp(a, b, VarExp(InitialEnv.VId_false))
fun SimplifyingAndalsoExp (a as VarExp vid, b) = if TypedSyntax.eqVId (vid, InitialEnv.VId_true) then
                                                     b
                                                 else if TypedSyntax.eqVId (vid, InitialEnv.VId_false) then
                                                     a
                                                 else
                                                     AndalsoExp(a, b)
  | SimplifyingAndalsoExp (a, b as VarExp vid) = if TypedSyntax.eqVId (vid, InitialEnv.VId_true) then
                                                     a
                                                 else
                                                     AndalsoExp(a, b)
  | SimplifyingAndalsoExp(a, b) = AndalsoExp(a, b)
fun EqualityType t = FnType (PairType (t, t), TyVar (tyNameToTyVar (Typing.primTyName_bool))) (* t * t -> bool *)
fun arityToKind 0 = TypeKind
  | arityToKind n = ArrowKind (TypeKind, arityToKind (n - 1))

(* occurCheck : TyVar -> Ty -> bool *)
fun occurCheck tv =
    let fun check (TyVar tv') = TypedSyntax.eqUTyVar (tv, tv')
          | check (RecordType xs) = Syntax.LabelMap.exists check xs
          | check (AppType { applied, arg }) = check applied orelse check arg
          | check (FnType(ty1, ty2)) = check ty1 orelse check ty2
          | check (ForallType (tv', kind, ty)) = if TypedSyntax.eqUTyVar (tv, tv') then
                                                     false
                                                 else
                                                     check ty
          | check (ExistsType (tv', kind, ty)) = if TypedSyntax.eqUTyVar (tv, tv') then
                                                     false
                                                 else
                                                     check ty
          | check (TypeFn (tv', kind, ty)) = if TypedSyntax.eqUTyVar (tv, tv') then
                                                 false
                                             else
                                                 check ty
          | check (SigType { valMap, strMap, exnTags }) = Syntax.VIdMap.exists (check o #1) valMap orelse Syntax.StrIdMap.exists check strMap
    in check
    end

(* substituteTy : TyVar * Ty -> Ty -> Ty *)
fun substituteTy (tv, replacement) =
    let fun go (ty as TyVar tv') = if TypedSyntax.eqUTyVar (tv, tv') then
                                       replacement
                                   else
                                       ty
          | go (RecordType fields) = RecordType (Syntax.LabelMap.map go fields)
          | go (AppType { applied, arg }) = AppType { applied = go applied, arg = go arg }
          | go (FnType(ty1, ty2)) = FnType(go ty1, go ty2)
          | go (ty as ForallType (tv', kind, ty')) = if TypedSyntax.eqUTyVar (tv, tv') then
                                                         ty
                                                     else if occurCheck tv' replacement then
                                                         (* TODO: generate fresh type variable *)
                                                         let val tv'' = raise Fail "FSyntax.substituteTy: not implemented yet"
                                                         in ForallType (tv'', kind, go (substituteTy (tv', TyVar tv'') ty'))
                                                         end
                                                     else
                                                         ForallType (tv', kind, go ty')
          | go (ty as ExistsType (tv', kind, ty')) = if TypedSyntax.eqUTyVar (tv, tv') then
                                                         ty
                                                     else if occurCheck tv' replacement then
                                                         (* TODO: generate fresh type variable *)
                                                         let val tv'' = raise Fail "FSyntax.substituteTy: not implemented yet"
                                                         in ExistsType (tv'', kind, go (substituteTy (tv', TyVar tv'') ty'))
                                                         end
                                                     else
                                                         ExistsType (tv', kind, go ty')
          | go (ty as TypeFn (tv', kind, ty')) = if TypedSyntax.eqUTyVar (tv, tv') then
                                                     ty
                                                 else if occurCheck tv' replacement then
                                                     (* TODO: generate fresh type variable *)
                                                     let val tv'' = raise Fail "FSyntax.substituteTy: not implemented yet"
                                                     in TypeFn (tv'', kind, go (substituteTy (tv', TyVar tv'') ty'))
                                                     end
                                                 else
                                                     TypeFn (tv', kind, go ty')
          | go (SigType { valMap, strMap, exnTags }) = SigType { valMap = Syntax.VIdMap.map (fn (ty, ids) => (go ty, ids)) valMap
                                                               , strMap = Syntax.StrIdMap.map go strMap
                                                               , exnTags = exnTags
                                                               }
    in go
    end

(* substTy : Ty TyVarMap.map -> { doTy : Ty -> Ty, doConBind : ConBind -> ConBind, doPat : Pat -> Pat, doExp : Exp -> Exp, doDec : Dec -> Dec, doDecs : Decs -> Decs } *)
fun substTy (subst : Ty TypedSyntax.TyVarMap.map) =
    let fun doTy (ty as TyVar tv) = (case TypedSyntax.TyVarMap.find (subst, tv) of
                                         NONE => ty
                                       | SOME replacement => replacement
                                    )
          | doTy (RecordType fields) = RecordType (Syntax.LabelMap.map doTy fields)
          | doTy (AppType { applied, arg }) = AppType { applied = doTy applied, arg = doTy arg }
          | doTy (FnType (ty1, ty2)) = FnType (doTy ty1, doTy ty2)
          | doTy (ForallType (tv, kind, ty)) = if TypedSyntax.TyVarMap.inDomain (subst, tv) then (* TODO: use fresh tyvar if necessary *)
                                                   ForallType (tv, kind, #doTy (substTy (#1 (TypedSyntax.TyVarMap.remove (subst, tv)))) ty)
                                               else
                                                   ForallType (tv, kind, doTy ty)
          | doTy (ExistsType (tv, kind, ty)) = if TypedSyntax.TyVarMap.inDomain (subst, tv) then (* TODO: use fresh tyvar if necessary *)
                                                   ExistsType (tv, kind, #doTy (substTy (#1 (TypedSyntax.TyVarMap.remove (subst, tv)))) ty)
                                               else
                                                   ExistsType (tv, kind, doTy ty)
          | doTy (TypeFn (tv, kind, ty)) = if TypedSyntax.TyVarMap.inDomain (subst, tv) then (* TODO: use fresh tyvar if necessary *)
                                               TypeFn (tv, kind, #doTy (substTy (#1 (TypedSyntax.TyVarMap.remove (subst, tv)))) ty)
                                           else
                                               TypeFn (tv, kind, doTy ty)
          | doTy (SigType { valMap, strMap, exnTags }) = SigType { valMap = Syntax.VIdMap.map (fn (ty, ids) => (doTy ty, ids)) valMap
                                                                 , strMap = Syntax.StrIdMap.map doTy strMap
                                                                 , exnTags = exnTags
                                                                 }
        fun doPat (pat as WildcardPat _) = pat
          | doPat (pat as SConPat _) = pat
          | doPat (VarPat (span, vid, ty)) = VarPat (span, vid, doTy ty)
          | doPat (RecordPat { sourceSpan, fields, ellipsis }) = RecordPat { sourceSpan = sourceSpan, fields = List.map (fn (label, pat) => (label, doPat pat)) fields, ellipsis = Option.map doPat ellipsis }
          | doPat (ConPat (span, path, optPat, tyargs)) = ConPat (span, path, Option.map doPat optPat, List.map doTy tyargs)
          | doPat (LayeredPat (span, vid, ty, pat)) = LayeredPat (span, vid, doTy ty, doPat pat)
          | doPat (VectorPat (span, pats, ellipsis, elemTy)) = VectorPat (span, Vector.map doPat pats, ellipsis, doTy elemTy)
        fun doConBind (ConBind (vid, optTy)) = ConBind (vid, Option.map doTy optTy)
        fun doDatBind (DatBind (tyvars, tyname, conbinds)) = let val subst' = List.foldl (fn (tv, subst) => if TypedSyntax.TyVarMap.inDomain (subst, tv) then #1 (TypedSyntax.TyVarMap.remove (subst, tv)) else subst) subst tyvars (* TODO: use fresh tyvar if necessary *)
                                                             in DatBind (tyvars, tyname, List.map (#doConBind (substTy subst')) conbinds)
                                                             end
        fun doExp (PrimExp (primOp, tyargs, args)) = PrimExp (primOp, Vector.map doTy tyargs, Vector.map doExp args)
          | doExp (exp as VarExp _) = exp
          | doExp (RecordExp fields) = RecordExp (List.map (fn (label, exp) => (label, doExp exp)) fields)
          | doExp (LetExp (dec, exp)) = LetExp (doDec dec, doExp exp)
          | doExp (AppExp (exp1, exp2)) = AppExp (doExp exp1, doExp exp2)
          | doExp (HandleExp { body, exnName, handler }) = HandleExp { body = doExp body, exnName = exnName, handler = doExp handler }
          | doExp (IfThenElseExp (exp1, exp2, exp3)) = IfThenElseExp (doExp exp1, doExp exp2, doExp exp3)
          | doExp (CaseExp (span, exp, ty, matches)) = CaseExp (span, doExp exp, doTy ty, List.map (fn (pat, exp) => (doPat pat, doExp exp)) matches)
          | doExp (FnExp (vid, ty, exp)) = FnExp (vid, doTy ty, doExp exp)
          | doExp (ProjectionExp { label, record }) = ProjectionExp { label = label, record = doExp record }
          | doExp (TyAbsExp (tv, kind, exp)) = if TypedSyntax.TyVarMap.inDomain (subst, tv) then (* TODO: use fresh tyvar if necessary *)
                                                   TyAbsExp (tv, kind, #doExp (substTy (#1 (TypedSyntax.TyVarMap.remove (subst, tv)))) exp)
                                               else
                                                   TyAbsExp (tv, kind, doExp exp)
          | doExp (PackExp { payloadTy, exp, packageTy }) = PackExp { payloadTy = doTy payloadTy, exp = doExp exp, packageTy = doTy packageTy }
          | doExp (TyAppExp (exp, ty)) = TyAppExp (doExp exp, doTy ty)
          | doExp (StructExp { valMap, strMap, exnTagMap }) = StructExp { valMap = valMap
                                                                        , strMap = strMap
                                                                        , exnTagMap = exnTagMap
                                                                        }
          | doExp (SProjectionExp (exp, label)) = SProjectionExp (doExp exp, label)
        and doDec (ValDec (vid, optTy, exp)) = ValDec (vid, Option.map doTy optTy, doExp exp)
          | doDec (RecValDec valbinds) = RecValDec (List.map (fn (vid, ty, exp) => (vid, doTy ty, doExp exp)) valbinds)
          | doDec (UnpackDec (tv, kind, vid, ty, exp)) = UnpackDec (tv, kind, vid, if TypedSyntax.TyVarMap.inDomain (subst, tv) then (* TODO: use fresh tyvar if necessary *)
                                                                                       #doTy (substTy (#1 (TypedSyntax.TyVarMap.remove (subst, tv)))) ty
                                                                                   else
                                                                                       doTy ty, doExp exp)
          | doDec (IgnoreDec exp) = IgnoreDec (doExp exp)
          | doDec (DatatypeDec datbinds) = DatatypeDec (List.map doDatBind datbinds)
          | doDec (ExceptionDec { conName, tagName, payloadTy }) = ExceptionDec { conName = conName, tagName = tagName, payloadTy = Option.map doTy payloadTy }
          | doDec (ExceptionRepDec { conName, conPath, tagPath, payloadTy }) = ExceptionRepDec { conName = conName, conPath = conPath, tagPath = tagPath, payloadTy = Option.map doTy payloadTy }
          | doDec (ExportValue exp) = ExportValue (doExp exp)
          | doDec (ExportModule fields) = ExportModule (Vector.map (fn (label, exp) => (label, doExp exp)) fields)
          | doDec (GroupDec (vars, decs)) = GroupDec (vars, List.map doDec decs)
    in { doTy = doTy
       , doConBind = doConBind
       , doPat = doPat
       , doExp = doExp
       , doDec = doDec
       , doDecs = List.map doDec
       }
    end

fun freeTyVarsInTy (bound : TypedSyntax.TyVarSet.set, TyVar tv) = if TypedSyntax.TyVarSet.member (bound, tv) then
                                                                      TypedSyntax.TyVarSet.empty
                                                                  else
                                                                      TypedSyntax.TyVarSet.singleton tv
  | freeTyVarsInTy (bound, RecordType fields) = Syntax.LabelMap.foldl (fn (ty, acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInTy (bound, ty))) TypedSyntax.TyVarSet.empty fields
  | freeTyVarsInTy (bound, AppType { applied, arg }) = TypedSyntax.TyVarSet.union (freeTyVarsInTy (bound, applied), freeTyVarsInTy (bound, arg))
  | freeTyVarsInTy (bound, FnType (ty1, ty2)) = TypedSyntax.TyVarSet.union (freeTyVarsInTy (bound, ty1), freeTyVarsInTy (bound, ty2))
  | freeTyVarsInTy (bound, ForallType (tv, kind, ty)) = freeTyVarsInTy (TypedSyntax.TyVarSet.add (bound, tv), ty)
  | freeTyVarsInTy (bound, ExistsType (tv, kind, ty)) = freeTyVarsInTy (TypedSyntax.TyVarSet.add (bound, tv), ty)
  | freeTyVarsInTy (bound, TypeFn (tv, kind, ty)) = freeTyVarsInTy (TypedSyntax.TyVarSet.add (bound, tv), ty)
  | freeTyVarsInTy (bound, SigType { valMap, strMap, exnTags }) = let val acc = Syntax.VIdMap.foldl (fn ((ty, ids), acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInTy (bound, ty))) TypedSyntax.TyVarSet.empty valMap
                                                                  in Syntax.StrIdMap.foldl (fn (ty, acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInTy (bound, ty))) acc strMap
                                                                  end
fun freeTyVarsInPat (bound, WildcardPat _) = TypedSyntax.TyVarSet.empty
  | freeTyVarsInPat (bound, SConPat _) = TypedSyntax.TyVarSet.empty
  | freeTyVarsInPat (bound, VarPat (_, vid, ty)) = freeTyVarsInTy (bound, ty)
  | freeTyVarsInPat (bound, RecordPat { sourceSpan, fields, ellipsis = NONE }) = List.foldl (fn ((label, pat), acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInPat (bound, pat))) TypedSyntax.TyVarSet.empty fields
  | freeTyVarsInPat (bound, RecordPat { sourceSpan, fields, ellipsis = SOME basePat }) = List.foldl (fn ((label, pat), acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInPat (bound, pat))) (freeTyVarsInPat (bound, basePat)) fields
  | freeTyVarsInPat (bound, ConPat (_, path, NONE, tyargs)) = List.foldl (fn (ty, acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInTy (bound, ty))) TypedSyntax.TyVarSet.empty tyargs
  | freeTyVarsInPat (bound, ConPat (_, path, SOME innerPat, tyargs)) = List.foldl (fn (ty, acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInTy (bound, ty))) (freeTyVarsInPat (bound, innerPat)) tyargs
  | freeTyVarsInPat (bound, LayeredPat (_, _, ty, innerPat)) = TypedSyntax.TyVarSet.union (freeTyVarsInTy (bound, ty), freeTyVarsInPat (bound, innerPat))
  | freeTyVarsInPat (bound, VectorPat (_, pats, ellipsis, elemTy)) = Vector.foldr (fn (pat, acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInPat (bound, pat))) (freeTyVarsInTy (bound, elemTy)) pats
fun freeTyVarsInExp (bound : TypedSyntax.TyVarSet.set, PrimExp (primOp, tyargs, args)) = let val acc = Vector.foldl (fn (ty, acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInTy (bound, ty))) TypedSyntax.TyVarSet.empty tyargs
                                                                                         in Vector.foldl (fn (exp, acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInExp (bound, exp))) acc args
                                                                                         end
  | freeTyVarsInExp (bound, VarExp _) = TypedSyntax.TyVarSet.empty
  | freeTyVarsInExp (bound, RecordExp fields) = List.foldl (fn ((label, exp), acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInExp (bound, exp))) TypedSyntax.TyVarSet.empty fields
  | freeTyVarsInExp (bound, LetExp (dec, exp)) = let val (bound, set) = freeTyVarsInDec (bound, dec)
                                                 in TypedSyntax.TyVarSet.union (set, freeTyVarsInExp (bound, exp))
                                                 end
  | freeTyVarsInExp (bound, AppExp (exp1, exp2)) = TypedSyntax.TyVarSet.union (freeTyVarsInExp (bound, exp1), freeTyVarsInExp (bound, exp2))
  | freeTyVarsInExp (bound, HandleExp { body, exnName, handler }) = TypedSyntax.TyVarSet.union (freeTyVarsInExp (bound, body), freeTyVarsInExp (bound, handler))
  | freeTyVarsInExp (bound, IfThenElseExp (exp1, exp2, exp3)) = TypedSyntax.TyVarSet.union (freeTyVarsInExp (bound, exp1), TypedSyntax.TyVarSet.union (freeTyVarsInExp (bound, exp2), freeTyVarsInExp (bound, exp3)))
  | freeTyVarsInExp (bound, CaseExp (span, exp, ty, matches)) = let val acc = freeTyVarsInExp (bound, exp)
                                                                    val acc = TypedSyntax.TyVarSet.union (acc, freeTyVarsInTy (bound, ty))
                                                                in List.foldl (fn ((pat, exp), acc) => TypedSyntax.TyVarSet.union (TypedSyntax.TyVarSet.union (acc, freeTyVarsInPat (bound, pat)), freeTyVarsInExp (bound, exp))) acc matches
                                                                end
  | freeTyVarsInExp (bound, FnExp (vid, ty, exp)) = TypedSyntax.TyVarSet.union (freeTyVarsInTy (bound, ty), freeTyVarsInExp (bound, exp))
  | freeTyVarsInExp (bound, ProjectionExp { label, record }) = freeTyVarsInExp (bound, record)
  | freeTyVarsInExp (bound, TyAbsExp (tv, kind, exp)) = freeTyVarsInExp (TypedSyntax.TyVarSet.add (bound, tv), exp)
  | freeTyVarsInExp (bound, TyAppExp (exp, ty)) = TypedSyntax.TyVarSet.union (freeTyVarsInExp (bound, exp), freeTyVarsInTy (bound, ty))
  | freeTyVarsInExp (bound, StructExp { valMap, strMap, exnTagMap }) = TypedSyntax.TyVarSet.empty
  | freeTyVarsInExp (bound, SProjectionExp (exp, label)) = freeTyVarsInExp (bound, exp)
  | freeTyVarsInExp (bound, PackExp { payloadTy, exp, packageTy }) = TypedSyntax.TyVarSet.union (TypedSyntax.TyVarSet.union (freeTyVarsInTy (bound, payloadTy), freeTyVarsInTy (bound, packageTy)), freeTyVarsInExp (bound, exp))
and freeTyVarsInDec (bound, ValDec (vid, optTy, exp)) = (bound, (case optTy of
                                                                     NONE => freeTyVarsInExp (bound, exp)
                                                                   | SOME ty => TypedSyntax.TyVarSet.union (freeTyVarsInTy (bound, ty), freeTyVarsInExp (bound, exp))
                                                                )
                                                        )
  | freeTyVarsInDec (bound, RecValDec valbinds) = (bound, List.foldl (fn ((vid, ty, exp), acc) => TypedSyntax.TyVarSet.union (TypedSyntax.TyVarSet.union (acc, freeTyVarsInTy (bound, ty)), freeTyVarsInExp (bound, exp))) TypedSyntax.TyVarSet.empty valbinds)
  | freeTyVarsInDec (bound, UnpackDec (tv, kind, vid, ty, exp)) = let val set1 = freeTyVarsInExp (bound, exp)
                                                                      val bound = TypedSyntax.TyVarSet.add (bound, tv)
                                                                  in (bound, TypedSyntax.TyVarSet.union (set1, freeTyVarsInTy (bound, ty)))
                                                                  end
  | freeTyVarsInDec (bound, IgnoreDec exp) = (bound, freeTyVarsInExp (bound, exp))
  | freeTyVarsInDec (bound, DatatypeDec datbinds) = let val bound = List.foldl (fn (DatBind (tyvars, tyname, conbinds), bound) => TypedSyntax.TyVarSet.add (bound, tyname)) bound datbinds
                                                    in (bound, List.foldl (fn (DatBind (tyvars, tyname, conbinds), acc) =>
                                                                              let val bound = TypedSyntax.TyVarSet.addList (bound, tyvars)
                                                                              in List.foldl (fn (ConBind (vid, NONE), acc) => acc
                                                                                            | (ConBind (vid, SOME ty), acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInTy (bound, ty))
                                                                                            ) acc conbinds
                                                                              end
                                                                          ) TypedSyntax.TyVarSet.empty datbinds)
                                                    end
  | freeTyVarsInDec (bound, ExceptionDec { conName, tagName, payloadTy }) = (bound, case payloadTy of
                                                                                        NONE => TypedSyntax.TyVarSet.empty
                                                                                      | SOME payloadTy => freeTyVarsInTy (bound, payloadTy)
                                                                            )
  | freeTyVarsInDec (bound, ExceptionRepDec { conName, conPath, tagPath, payloadTy }) = (bound, case payloadTy of
                                                                                                    NONE => TypedSyntax.TyVarSet.empty
                                                                                                  | SOME payloadTy => freeTyVarsInTy (bound, payloadTy)
                                                                                        )
  | freeTyVarsInDec (bound, ExportValue exp) = (bound, freeTyVarsInExp (bound, exp))
  | freeTyVarsInDec (bound, ExportModule exports) = (bound, Vector.foldl (fn ((name, exp), acc) => TypedSyntax.TyVarSet.union (acc, freeTyVarsInExp (bound, exp))) TypedSyntax.TyVarSet.empty exports)
  | freeTyVarsInDec (bound, GroupDec (v, decs)) = freeTyVarsInDecs (bound, decs)
and freeTyVarsInDecs (bound, decs) = List.foldl (fn (dec, (bound, acc)) => let val (bound, vars) = freeTyVarsInDec (bound, dec)
                                                                           in (bound, TypedSyntax.TyVarSet.union (acc, vars))
                                                                           end
                                                ) (bound, TypedSyntax.TyVarSet.empty) decs

fun varsInPat (WildcardPat _) = TypedSyntax.VIdSet.empty
  | varsInPat (SConPat _) = TypedSyntax.VIdSet.empty
  | varsInPat (VarPat (_, vid, ty)) = TypedSyntax.VIdSet.singleton vid
  | varsInPat (RecordPat { sourceSpan, fields, ellipsis = NONE }) = List.foldl (fn ((label, pat), acc) => TypedSyntax.VIdSet.union (acc, varsInPat pat)) TypedSyntax.VIdSet.empty fields
  | varsInPat (RecordPat { sourceSpan, fields, ellipsis = SOME basePat }) = List.foldl (fn ((label, pat), acc) => TypedSyntax.VIdSet.union (acc, varsInPat pat)) (varsInPat basePat) fields
  | varsInPat (ConPat (_, conPath, SOME innerPat, tyargs)) = varsInPat innerPat
  | varsInPat (ConPat (_, conPath, NONE, tyargs)) = TypedSyntax.VIdSet.empty
  | varsInPat (LayeredPat (_, vid, ty, innerPat)) = TypedSyntax.VIdSet.add (varsInPat innerPat, vid)
  | varsInPat (VectorPat (_, pats, wildcard, ty)) = Vector.foldl (fn (pat, acc) => TypedSyntax.VIdSet.union (acc, varsInPat pat)) TypedSyntax.VIdSet.empty pats

fun freeVarsInExp (bound : TypedSyntax.VIdSet.set, PrimExp (primOp, tyargs, args)) = Vector.foldl (fn (exp, acc) => TypedSyntax.VIdSet.union (acc, freeVarsInExp (bound, exp))) TypedSyntax.VIdSet.empty args
  | freeVarsInExp (bound, VarExp vid) = if TypedSyntax.VIdSet.member (bound, vid) then
                                            TypedSyntax.VIdSet.empty
                                        else
                                            TypedSyntax.VIdSet.singleton vid
  | freeVarsInExp (bound, RecordExp fields) = List.foldl (fn ((label, exp), acc) => TypedSyntax.VIdSet.union (acc, freeVarsInExp (bound, exp))) TypedSyntax.VIdSet.empty fields
  | freeVarsInExp (bound, LetExp (dec, exp)) = let val (bound, set) = freeVarsInDec (bound, dec)
                                               in TypedSyntax.VIdSet.union (set, freeVarsInExp (bound, exp))
                                               end
  | freeVarsInExp (bound, AppExp (exp1, exp2)) = TypedSyntax.VIdSet.union (freeVarsInExp (bound, exp1), freeVarsInExp (bound, exp2))
  | freeVarsInExp (bound, HandleExp { body, exnName, handler }) = TypedSyntax.VIdSet.union (freeVarsInExp (bound, body), freeVarsInExp (TypedSyntax.VIdSet.add (bound, exnName), handler))
  | freeVarsInExp (bound, IfThenElseExp (exp1, exp2, exp3)) = TypedSyntax.VIdSet.union (TypedSyntax.VIdSet.union (freeVarsInExp (bound, exp1), freeVarsInExp (bound, exp2)), freeVarsInExp (bound, exp3))
  | freeVarsInExp (bound, CaseExp (span, exp, ty, matches)) = List.foldl (fn ((pat, exp), acc) => TypedSyntax.VIdSet.union (acc, freeVarsInExp (TypedSyntax.VIdSet.union (bound, varsInPat pat), exp))) (freeVarsInExp (bound, exp)) matches
  | freeVarsInExp (bound, FnExp (vid, ty, exp)) = freeVarsInExp (TypedSyntax.VIdSet.add (bound, vid), exp)
  | freeVarsInExp (bound, ProjectionExp { label, record }) = freeVarsInExp (bound, record)
  | freeVarsInExp (bound, TyAbsExp (tv, kind, exp)) = freeVarsInExp (bound, exp)
  | freeVarsInExp (bound, TyAppExp (exp, ty)) = freeVarsInExp (bound, exp)
  | freeVarsInExp (bound, StructExp { valMap, strMap, exnTagMap }) = let fun addPath (path, set) = TypedSyntax.VIdSet.add (set, rootOfPath path)
                                                                     in List.foldl TypedSyntax.VIdSet.union TypedSyntax.VIdSet.empty
                                                                                   [Syntax.VIdMap.foldl addPath TypedSyntax.VIdSet.empty valMap
                                                                                   ,Syntax.StrIdMap.foldl addPath TypedSyntax.VIdSet.empty strMap
                                                                                   ,Syntax.VIdMap.foldl addPath TypedSyntax.VIdSet.empty exnTagMap
                                                                                   ]
                                                                     end
  | freeVarsInExp (bound, SProjectionExp (exp, label)) = freeVarsInExp (bound, exp)
  | freeVarsInExp (bound, PackExp { payloadTy, exp, packageTy }) = freeVarsInExp (bound, exp)
and freeVarsInDec (bound, ValDec (vid, ty, exp)) = (TypedSyntax.VIdSet.add (bound, vid), freeVarsInExp (bound, exp))
  | freeVarsInDec (bound, RecValDec valbinds) = let val bound = List.foldl (fn ((vid, _, _), bound) => TypedSyntax.VIdSet.add (bound, vid)) bound valbinds
                                                in (bound, List.foldl (fn ((_, _, exp), acc) => TypedSyntax.VIdSet.union (acc, freeVarsInExp (bound, exp))) TypedSyntax.VIdSet.empty valbinds)
                                                end
  | freeVarsInDec (bound, UnpackDec (tv, kind, vid, ty, exp)) = (TypedSyntax.VIdSet.add (bound, vid), freeVarsInExp (bound, exp))
  | freeVarsInDec (bound, IgnoreDec exp) = (bound, freeVarsInExp (bound, exp))
  | freeVarsInDec (bound, DatatypeDec datbinds) = (List.foldl (fn (DatBind (tyvars, tyname, conbinds), bound) => List.foldl (fn (ConBind (vid, optTy), bound) => TypedSyntax.VIdSet.add (bound, vid)) bound conbinds) bound datbinds, TypedSyntax.VIdSet.empty)
  | freeVarsInDec (bound, ExceptionDec { conName, tagName, payloadTy }) = (TypedSyntax.VIdSet.add (TypedSyntax.VIdSet.add (bound, conName), tagName), TypedSyntax.VIdSet.empty)
  | freeVarsInDec (bound, ExceptionRepDec { conName, conPath, tagPath, payloadTy }) = (TypedSyntax.VIdSet.add (bound, conName), TypedSyntax.VIdSet.empty)
  | freeVarsInDec (bound, ExportValue exp) = (bound, freeVarsInExp (bound, exp))
  | freeVarsInDec (bound, ExportModule exps) = (bound, Vector.foldl (fn ((name, exp), acc) => TypedSyntax.VIdSet.union (acc, freeVarsInExp (bound, exp))) TypedSyntax.VIdSet.empty exps)
  | freeVarsInDec (bound, GroupDec (_, decs)) = List.foldl (fn (dec, (bound, acc)) => let val (bound, set) = freeVarsInDec (bound, dec)
                                                                                      in (bound, TypedSyntax.VIdSet.union (acc, set))
                                                                                      end) (bound, TypedSyntax.VIdSet.empty) decs

local
    fun isLongStrId (VarExp (TypedSyntax.MkVId (name, n)), TypedSyntax.MkStrId (name', n'), []) = n = n' andalso name = name'
      | isLongStrId(SProjectionExp(exp, StructLabel strid), strid0, stridLast :: strids) = strid = stridLast andalso isLongStrId(exp, strid0, strids)
      | isLongStrId(_, _, _) = false
in
    fun isLongVId (VarExp vid, TypedSyntax.MkShortVId vid') = TypedSyntax.eqVId (vid, vid')
      | isLongVId (SProjectionExp (exp, ValueLabel vid), TypedSyntax.MkLongVId (strid0, strids, vid')) = vid = vid' andalso isLongStrId (exp, strid0, List.rev strids)
      | isLongVId(_, _) = false
end

structure PrettyPrint = struct
val print_TyVar = TypedSyntax.print_TyVar
val print_VId = TypedSyntax.print_VId
val print_LongVId = TypedSyntax.print_LongVId
fun print_Path (Root vid) = TypedSyntax.print_VId vid
  | print_Path (Child (parent, label)) = print_Path parent ^ "/.." (* TODO *)
  | print_Path (Field (parent, label)) = print_Path parent ^ "/.." (* TODO *)
fun print_Ty (TyVar x) = "TyVar(" ^ print_TyVar x ^ ")"
  | print_Ty (RecordType xs) = let val xs = Syntax.LabelMap.listItemsi xs
                               in case Syntax.extractTuple (1, xs) of
                                      NONE => "RecordType " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label,print_Ty)) xs
                                    | SOME ys => "TupleType " ^ Syntax.print_list print_Ty ys
                               end
  | print_Ty (AppType { applied, arg }) = "AppType{applied=" ^ print_Ty applied ^ ",arg=" ^ print_Ty arg ^ "}"
  | print_Ty (FnType(x,y)) = "FnType(" ^ print_Ty x ^ "," ^ print_Ty y ^ ")"
  | print_Ty (ForallType(tv,kind,x)) = "ForallType(" ^ print_TyVar tv ^ "," ^ print_Ty x ^ ")"
  | print_Ty (ExistsType(tv,kind,x)) = "ExistsType(" ^ print_TyVar tv ^ "," ^ print_Ty x ^ ")"
  | print_Ty (TypeFn(tv,kind,x)) = "TypeFn(" ^ print_TyVar tv ^ "," ^ print_Ty x ^ ")"
  | print_Ty (SigType _) = "SigType"
fun print_Pat (WildcardPat _) = "WildcardPat"
  | print_Pat (SConPat { sourceSpan, scon, equality, cookedValue }) = "SConPat(" ^ Syntax.print_SCon scon ^ ")"
  | print_Pat (VarPat(_, vid, ty)) = "VarPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ ")"
  | print_Pat (LayeredPat (_, vid, ty, pat)) = "TypedPat(" ^ print_VId vid ^ "," ^ print_Ty ty ^ "," ^ print_Pat pat ^ ")"
  | print_Pat (ConPat(_, path, pat, tyargs)) = "ConPat(" ^ print_Path path ^ "," ^ Syntax.print_option print_Pat pat ^ "," ^ Syntax.print_list print_Ty tyargs ^ ")"
  | print_Pat (RecordPat { sourceSpan, fields, ellipsis = NONE })
    = (case Syntax.extractTuple (1, fields) of
           NONE => "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) fields ^ ",NONE)"
         | SOME ys => "TuplePat " ^ Syntax.print_list print_Pat ys
      )
  | print_Pat (RecordPat { sourceSpan, fields, ellipsis = SOME basePat }) = "RecordPat(" ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Pat)) fields ^ ",SOME(" ^ print_Pat basePat ^ "))"
  | print_Pat (VectorPat _) = "VectorPat"
fun print_PrimOp (IntConstOp x) = "IntConstOp " ^ IntInf.toString x
  | print_PrimOp (WordConstOp x) = "WordConstOp " ^ IntInf.toString x
  | print_PrimOp (RealConstOp x) = "RealConstOp " ^ Numeric.Notation.toString "~" x
  | print_PrimOp (StringConstOp x) = "StringConstOp \"" ^ Vector.foldr (fn (c, acc) => StringElement.charToString (StringElement.CODEUNIT c) ^ acc) "\"" x
  | print_PrimOp (CharConstOp x) = "CharConstOp \"" ^ StringElement.charToString (StringElement.CODEUNIT x) ^ "\""
  | print_PrimOp (RaiseOp span) = "RaiseOp"
  | print_PrimOp ListOp = "ListOp"
  | print_PrimOp VectorOp = "VectorOp"
  | print_PrimOp RecordEqualityOp = "RecordEqualityOp"
  | print_PrimOp DataTagOp = "DataTagOp"
  | print_PrimOp DataPayloadOp = "DataPayloadOp"
  | print_PrimOp ExnInstanceofOp = "ExnInstanceofOp"
  | print_PrimOp (PrimFnOp x) = Primitives.toString x
fun print_Exp (PrimExp (primOp, tyargs, args)) = "PrimExp(" ^ print_PrimOp primOp ^ "," ^ String.concatWith "," (Vector.foldr (fn (x, xs) => print_Ty x :: xs) [] tyargs) ^ "," ^ String.concatWith "," (Vector.foldr (fn (x, xs) => print_Exp x :: xs) [] args) ^ ")"
  | print_Exp (VarExp(x)) = "VarExp(" ^ print_VId x ^ ")"
  | print_Exp (RecordExp x) = (case Syntax.extractTuple (1, x) of
                                   NONE => "RecordExp " ^ Syntax.print_list (Syntax.print_pair (Syntax.print_Label, print_Exp)) x
                                 | SOME ys => "TupleExp " ^ Syntax.print_list print_Exp ys
                              )
  | print_Exp (LetExp(dec,x)) = "LetExp(" ^ print_Dec dec ^ "," ^ print_Exp x ^ ")"
  | print_Exp (AppExp(x,y)) = "AppExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ ")"
  | print_Exp (HandleExp { body, exnName, handler }) = "HandleExp{body=" ^ print_Exp body ^ ",exnName=" ^ TypedSyntax.print_VId exnName ^ ",handler=" ^ print_Exp handler ^ ")"
  | print_Exp (IfThenElseExp(x,y,z)) = "IfThenElseExp(" ^ print_Exp x ^ "," ^ print_Exp y ^ "," ^ print_Exp z ^ ")"
  | print_Exp (CaseExp(_,x,ty,y)) = "CaseExp(" ^ print_Exp x ^ "," ^ print_Ty ty ^ "," ^ Syntax.print_list (Syntax.print_pair (print_Pat,print_Exp)) y ^ ")"
  | print_Exp (FnExp(pname,pty,body)) = "FnExp(" ^ print_VId pname ^ "," ^ print_Ty pty ^ "," ^ print_Exp body ^ ")"
  | print_Exp (ProjectionExp { label, record }) = "ProjectionExp{label=" ^ Syntax.print_Label label ^ ",record=" ^ print_Exp record ^ "}"
  | print_Exp (TyAbsExp(tv, kind, exp)) = "TyAbsExp(" ^ print_TyVar tv ^ "," ^ print_Exp exp ^ ")"
  | print_Exp (TyAppExp(exp, ty)) = "TyAppExp(" ^ print_Exp exp ^ "," ^ print_Ty ty ^ ")"
  | print_Exp (StructExp { valMap, strMap, exnTagMap }) = "StructExp{valMap={" ^ Syntax.VIdMap.foldri (fn (vid,path,acc) => Syntax.print_VId vid ^ ":" ^ print_Path path ^ ";" ^ acc) "" valMap ^ "},strMap={" ^ Syntax.StrIdMap.foldri (fn (strid,path,acc) => Syntax.print_StrId strid ^ ":" ^ print_Path path ^ ";" ^ acc) "" strMap ^ "},exnTagMap={" ^ Syntax.VIdMap.foldri (fn (vid,path,acc) => Syntax.print_VId vid ^ ":" ^ print_Path path ^ ";" ^ acc) "" exnTagMap ^ "}}"
  | print_Exp (SProjectionExp _) = "SProjectionExp"
  | print_Exp (PackExp { payloadTy, exp, packageTy }) = "PackExp{payloadTy=" ^ print_Ty payloadTy ^ ",exp=" ^ print_Exp exp ^ ",packageTy=" ^ print_Ty packageTy ^ "}"
and print_Dec (ValDec (vid, optTy, exp)) = (case optTy of
                                                SOME ty => "ValDec(" ^ print_VId vid ^ ",SOME " ^ print_Ty ty ^ "," ^ print_Exp exp ^ ")"
                                              | NONE => "ValDec(" ^ print_VId vid ^ ",NONE," ^ print_Exp exp ^ ")"
                                           )
  | print_Dec (RecValDec valbinds) = "RecValDec(" ^ Syntax.print_list (fn (vid, ty, exp) => "(" ^ print_VId vid ^ "," ^ print_Ty ty ^ "," ^ print_Exp exp ^ ")") valbinds ^ ")"
  | print_Dec (UnpackDec (tv, kind, vid, ty, exp)) = "UnpackDec(" ^ TypedSyntax.print_TyVar tv ^ "," ^ print_VId vid ^ "," ^ print_Ty ty ^ "," ^ print_Exp exp ^ ")"
  | print_Dec (IgnoreDec exp) = "IgnoreDec(" ^ print_Exp exp ^ ")"
  | print_Dec (DatatypeDec datbinds) = "DatatypeDec"
  | print_Dec (ExceptionDec _) = "ExceptionDec"
  | print_Dec (ExceptionRepDec _) = "ExceptionRepDec"
  | print_Dec (ExportValue _) = "ExportValue"
  | print_Dec (ExportModule _) = "ExportModule"
  | print_Dec (GroupDec (vids, decs)) = "GroupDec(" ^ Syntax.print_list print_Dec decs ^ ")"
val print_Decs = Syntax.print_list print_Dec
end (* structure PrettyPrint *)
end (* structure FSyntax *)

structure ToFSyntax = struct
exception Error of SourcePos.span list * string

fun LongStrIdExp (TypedSyntax.MkLongStrId (strid0, strids)) = List.foldl (fn (label, x) => FSyntax.SProjectionExp (x, FSyntax.StructLabel label)) (FSyntax.VarExp (FSyntax.strIdToVId strid0)) strids

fun LongVIdToPath (TypedSyntax.MkLongVId (strid0, strids, vid)) = FSyntax.Child (List.foldl (fn (strid, p) => FSyntax.Child (p, FSyntax.StructLabel strid)) (FSyntax.Root (FSyntax.strIdToVId strid0)) strids, FSyntax.ValueLabel vid)
  | LongVIdToPath (TypedSyntax.MkShortVId vid) = FSyntax.Root vid
fun LongStrIdToPath (TypedSyntax.MkLongStrId (strid0, strids)) = List.foldl (fn (strid, p) => FSyntax.Child (p, FSyntax.StructLabel strid)) (FSyntax.Root (FSyntax.strIdToVId strid0)) strids

type Context = { nextVId : int ref
               , nextTyVar : int ref
               }
fun emitError (ctx : Context, spans, message) = raise Error (spans, message)
type Env = { equalityForTyVarMap : TypedSyntax.VId TypedSyntax.TyVarMap.map
           , equalityForTyNameMap : TypedSyntax.LongVId TypedSyntax.TyNameMap.map
           , exnTagMap : FSyntax.Path TypedSyntax.LongVIdMap.map
           , overloadMap : (FSyntax.Exp Syntax.OverloadKeyMap.map) TypedSyntax.TyNameMap.map
           }
val initialEnv : Env = { equalityForTyVarMap = TypedSyntax.TyVarMap.empty
                       , equalityForTyNameMap = TypedSyntax.TyNameMap.empty
                       , exnTagMap = let open InitialEnv
                                     in List.foldl (fn ((con, tag), m) => TypedSyntax.LongVIdMap.insert (m, con, LongVIdToPath tag)) TypedSyntax.LongVIdMap.empty
                                                   [(LongVId_Match, VId_Match_tag)
                                                   ,(LongVId_Bind, VId_Bind_tag)
                                                   ,(LongVId_Div, VId_Div_tag)
                                                   ,(LongVId_Overflow, VId_Overflow_tag)
                                                   ,(LongVId_Size, VId_Size_tag)
                                                   ,(LongVId_Subscript, VId_Subscript_tag)
                                                   ,(LongVId_Fail, VId_Fail_tag)
                                                   ,(VId_Lua_LuaError, VId_Lua_LuaError_tag)
                                                   ]
                                     end
                       , overloadMap = TypedSyntax.TyNameMap.empty
                       }

fun updateEqualityForTyVarMap(f, env : Env) : Env = { equalityForTyVarMap = f (#equalityForTyVarMap env)
                                                    , equalityForTyNameMap = #equalityForTyNameMap env
                                                    , exnTagMap = #exnTagMap env
                                                    , overloadMap = #overloadMap env
                                                    }

fun updateEqualityForTyNameMap(f, env : Env) : Env = { equalityForTyVarMap = #equalityForTyVarMap env
                                                     , equalityForTyNameMap = f (#equalityForTyNameMap env)
                                                     , exnTagMap = #exnTagMap env
                                                     , overloadMap = #overloadMap env
                                                     }

fun updateExnTagMap(f, env : Env) : Env = { equalityForTyVarMap = #equalityForTyVarMap env
                                          , equalityForTyNameMap = #equalityForTyNameMap env
                                          , exnTagMap = f (#exnTagMap env)
                                          , overloadMap = #overloadMap env
                                          }

fun updateOverloadMap(f, env : Env) : Env = { equalityForTyVarMap = #equalityForTyVarMap env
                                            , equalityForTyNameMap = #equalityForTyNameMap env
                                            , exnTagMap = #exnTagMap env
                                            , overloadMap = f (#overloadMap env)
                                            }

fun freshTyVar(ctx : Context) = let val n = !(#nextTyVar ctx)
                                in #nextTyVar ctx := n + 1
                                 ; TypedSyntax.AnonymousTyVar n
                                end
fun freshVId(ctx : Context, name: string) = let val n = !(#nextVId ctx)
                                            in #nextVId ctx := n + 1
                                             ; TypedSyntax.MkVId (name, n)
                                            end

local structure U = TypedSyntax
      structure F = FSyntax
      (* toFTy : Context * Env * TypedSyntax.Ty -> FSyntax.Ty *)
      (* toFPat : Context * Env * TypedSyntax.Pat -> unit TypedSyntax.VIdMap.map * FSyntax.Pat *)
      (* toFExp : Context * Env * TypedSyntax.Exp -> FSyntax.Exp *)
      (* toFDecs : Context * Env * TypedSyntax.Dec list -> Env * FSyntax.Dec list *)
      (* getEquality : Context * Env * TypedSyntax.Ty -> FSyntax.Exp *)
      val overloads = let open InitialEnv Syntax
                      in List.foldl TypedSyntax.VIdMap.insert' TypedSyntax.VIdMap.empty
                                    [(VId_abs, OVERLOAD_abs)
                                    ,(VId_TILDE, OVERLOAD_TILDE)
                                    ,(VId_div, OVERLOAD_div)
                                    ,(VId_mod, OVERLOAD_mod)
                                    ,(VId_TIMES, OVERLOAD_TIMES)
                                    ,(VId_DIVIDE, OVERLOAD_DIVIDE)
                                    ,(VId_PLUS, OVERLOAD_PLUS)
                                    ,(VId_MINUS, OVERLOAD_MINUS)
                                    ,(VId_LT, OVERLOAD_LT)
                                    ,(VId_LE, OVERLOAD_LE)
                                    ,(VId_GT, OVERLOAD_GT)
                                    ,(VId_GE, OVERLOAD_GE)
                                    ]
                      end
in
fun toFTy (ctx : Context, env : Env, U.TyVar (span, tv)) = F.TyVar tv
  | toFTy (ctx, env, U.RecordType (span, fields)) = F.RecordType (Syntax.LabelMap.map (fn ty => toFTy (ctx, env, ty)) fields)
  | toFTy (ctx, env, U.TyCon (span, tyargs, tyname)) = F.TyCon (List.map (fn arg => toFTy (ctx, env, arg)) tyargs, tyname)
  | toFTy (ctx, env, U.FnType (span, paramTy, resultTy)) = let fun doTy ty = toFTy (ctx, env, ty)
                                                           in F.FnType (doTy paramTy, doTy resultTy)
                                                           end
fun cookIntegerConstant(ctx, env, span, value : IntInf.int, ty)
    = (case ty of
           U.TyCon(_, [], tycon) => if U.eqTyName (tycon, Typing.primTyName_int) then
                                        F.IntConstExp (value, toFTy (ctx, env, ty))
                                    else if U.eqTyName (tycon, Typing.primTyName_intInf) then
                                        F.IntConstExp (value, toFTy (ctx, env, ty))
                                    else
                                        let val overloadMap = case TypedSyntax.TyNameMap.find (#overloadMap env, tycon) of
                                                                  SOME m => m
                                                                | NONE => raise Fail "invalid integer constant"
                                            val fromInt = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_fromInt) of
                                                              SOME x => x
                                                            | NONE => raise Fail "invalid integer constant"
                                            val PLUS = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_PLUS) of
                                                           SOME x => x
                                                         | NONE => raise Fail "invalid integer constant"
                                            val TIMES = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_TIMES) of
                                                            SOME x => x
                                                          | NONE => raise Fail "invalid integer constant"
                                            val TILDE = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_TILDE) of
                                                            SOME x => x
                                                          | NONE => raise Fail "invalid integer constant"
                                            val intTy = toFTy (ctx, env, Typing.primTy_int)
                                            fun decompose x = if ~0x80000000 <= x andalso x <= 0x7fffffff then
                                                                  F.AppExp (fromInt, F.IntConstExp (x, intTy))
                                                              else
                                                                  let val (q, r) = IntInf.quotRem (x, ~0x80000000)
                                                                      val y = case q of
                                                                                  1 => F.IntConstExp (~0x80000000, intTy)
                                                                                | ~1 => F.AppExp (TILDE, F.IntConstExp (~0x80000000, intTy))
                                                                                | _ => F.AppExp (TIMES, F.TupleExp [decompose q, F.AppExp (fromInt, F.IntConstExp (~0x80000000, intTy))])
                                                                  in if r = 0 then
                                                                         y
                                                                     else
                                                                         F.AppExp (PLUS, F.TupleExp [y, F.AppExp (fromInt, F.IntConstExp (r, intTy))])
                                                                  end
                                        in decompose value
                                        end
         | _ => raise Fail "invalid integer constant"
      ) (* TODO: check range *)
fun cookWordConstant(ctx, env, span, value : IntInf.int, ty)
    = (case ty of
           U.TyCon(_, [], tycon) => if U.eqTyName (tycon, Typing.primTyName_word) then
                                        F.WordConstExp (value, toFTy (ctx, env, ty))
                                    else
                                        let val overloadMap = case TypedSyntax.TyNameMap.find (#overloadMap env, tycon) of
                                                                  SOME m => m
                                                                | NONE => raise Fail ("invalid word constant for " ^ TypedSyntax.print_TyName tycon)
                                            val fromWord = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_fromWord) of
                                                              SOME x => x
                                                            | NONE => raise Fail "invalid word constant: fromWord is not defined"
                                            val PLUS = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_PLUS) of
                                                           SOME x => x
                                                         | NONE => raise Fail "invalid word constant: + is not defined"
                                            val TIMES = case Syntax.OverloadKeyMap.find (overloadMap, Syntax.OVERLOAD_TIMES) of
                                                            SOME x => x
                                                          | NONE => raise Fail "invalid word constant: * is not defined"
                                            val wordTy = toFTy (ctx, env, Typing.primTy_word)
                                            fun decompose x = if x <= 0xffffffff then
                                                                  F.AppExp (fromWord, F.WordConstExp (x, wordTy))
                                                              else
                                                                  let val (q, r) = IntInf.quotRem (x, 0xffffffff)
                                                                      val y = case q of
                                                                                  1 => F.WordConstExp (0xffffffff, wordTy)
                                                                                | _ => F.AppExp (TIMES, F.TupleExp [decompose q, F.AppExp (fromWord, F.WordConstExp (0xffffffff, wordTy))])
                                                                  in if r = 0 then
                                                                         y
                                                                     else
                                                                         F.AppExp (PLUS, F.TupleExp [y, F.AppExp (fromWord, F.WordConstExp (r, wordTy))])
                                                                  end
                                        in decompose value
                                        end
         | _ => raise Fail "invalid word constant: invalid type"
      ) (* TODO: check range *)
fun cookCharacterConstant (ctx, env, span, value : int, ty)
    = (case ty of
           U.TyCon (_, [], tycon) => if U.eqTyName (tycon, Typing.primTyName_char) then
                                         if 0 <= value andalso value <= 255 then
                                             F.PrimExp (F.CharConstOp value, vector [toFTy (ctx, env, ty)], vector [])
                                         else
                                             raise Fail "invalid character constant: out of range"
                                     else if U.eqTyName (tycon, Typing.primTyName_wideChar) then
                                         if 0 <= value andalso value <= 0xffff then (* TODO: target dependence *)
                                             F.PrimExp (F.CharConstOp value, vector [toFTy (ctx, env, ty)], vector [])
                                         else
                                             raise Fail "invalid character constant: out of range"
                                     else
                                         raise Fail "invalid character constant: type"
         | _ => raise Fail "invalid character constant: type"
      )
fun cookStringConstant (ctx, env, span, value, ty)
    = (case ty of
           U.TyCon (_, [], tycon) => if U.eqTyName (tycon, Typing.primTyName_string) then
                                         let val cooked = StringElement.encode8bit value
                                                          handle Chr => raise Fail "invalid string constant: out of range"
                                         in F.PrimExp (F.StringConstOp cooked, vector [toFTy (ctx, env, ty)], vector [])
                                         end
                                     else if U.eqTyName (tycon, Typing.primTyName_wideString) then
                                         (* TODO: target dependence *)
                                         let val cooked = StringElement.encode16bit value
                                                          handle Chr => raise Fail "invalid string constant: out of range"
                                         in F.PrimExp (F.StringConstOp cooked, vector [toFTy (ctx, env, ty)], vector [])
                                         end
                                     else
                                         raise Fail "invalid string constant: type"
         | _ => raise Fail "invalid string constant: type"
      )
fun toFPat (ctx, env, U.WildcardPat span) = (TypedSyntax.VIdMap.empty, F.WildcardPat span)
  | toFPat(ctx, env, U.SConPat(span, scon as Syntax.IntegerConstant value, ty))
    = (TypedSyntax.VIdMap.empty, F.SConPat { sourceSpan = span
                                           , scon = scon
                                           , equality = getEquality (ctx, env, ty)
                                           , cookedValue = cookIntegerConstant (ctx, env, span, value, ty)
                                           }
      )
  | toFPat(ctx, env, U.SConPat(span, scon as Syntax.WordConstant value, ty))
    = (TypedSyntax.VIdMap.empty, F.SConPat { sourceSpan = span
                                           , scon = scon
                                           , equality = getEquality (ctx, env, ty)
                                           , cookedValue = cookWordConstant (ctx, env, span, value, ty)
                                           }
      )
  | toFPat(ctx, env, U.SConPat(span, scon as Syntax.RealConstant value, ty)) = raise Fail "invalid real constant in pattern"
  | toFPat(ctx, env, U.SConPat(span, scon as Syntax.CharacterConstant value, ty))
    = (TypedSyntax.VIdMap.empty, F.SConPat { sourceSpan = span
                                           , scon = scon
                                           , equality = getEquality (ctx, env, ty)
                                           , cookedValue = cookCharacterConstant (ctx, env, span, value, ty)
                                           }
      )
  | toFPat(ctx, env, U.SConPat(span, scon as Syntax.StringConstant value, ty))
    = (TypedSyntax.VIdMap.empty, F.SConPat { sourceSpan = span
                                           , scon = scon
                                           , equality = getEquality (ctx, env, ty)
                                           , cookedValue = cookStringConstant (ctx, env, span, value, ty)
                                           }
      )
  | toFPat (ctx, env, U.VarPat (span, vid, ty)) = (TypedSyntax.VIdMap.empty, F.VarPat (span, vid, toFTy (ctx, env, ty))) (* TODO *)
  | toFPat(ctx, env, U.RecordPat { sourceSpan, fields, ellipsis })
    = let fun doField(label, pat) = let val (_, pat') = toFPat(ctx, env, pat)
                                    in (label, pat')
                                    end
      in (TypedSyntax.VIdMap.empty (* TODO *), F.RecordPat { sourceSpan = sourceSpan
                                                           , fields = List.map doField fields
                                                           , ellipsis = Option.map (fn pat => #2 (toFPat (ctx, env, pat))) ellipsis
                                                           }
         )
      end
  | toFPat(ctx, env, U.ConPat { sourceSpan = span, longvid, payload = NONE, tyargs, isSoleConstructor })
    = (TypedSyntax.VIdMap.empty, F.ConPat (span, LongVIdToPath longvid, NONE, List.map (fn ty => toFTy (ctx, env, ty)) tyargs))
  | toFPat(ctx, env, U.ConPat { sourceSpan = span, longvid, payload = SOME payloadPat, tyargs, isSoleConstructor })
    = let val (m, payloadPat') = toFPat(ctx, env, payloadPat)
      in (TypedSyntax.VIdMap.empty, F.ConPat (span, LongVIdToPath longvid, SOME payloadPat', List.map (fn ty => toFTy (ctx, env, ty)) tyargs))
      end
  | toFPat(ctx, env, U.TypedPat(_, pat, _)) = toFPat(ctx, env, pat)
  | toFPat(ctx, env, U.LayeredPat(span, vid, ty, innerPat)) = let val (m, innerPat') = toFPat(ctx, env, innerPat)
                                                              in (TypedSyntax.VIdMap.empty, F.LayeredPat (span, vid, toFTy (ctx, env, ty), innerPat')) (* TODO *)
                                                              end
  | toFPat(ctx, env, U.VectorPat(span, pats, ellipsis, elemTy)) = let val pats = Vector.map (fn pat => toFPat(ctx, env, pat)) pats
                                                                  in (TypedSyntax.VIdMap.empty, F.VectorPat (span, Vector.map #2 pats, ellipsis, toFTy (ctx, env, elemTy)))
                                                                  end
and toFExp (ctx, env, U.SConExp (span, Syntax.IntegerConstant value, ty)) = cookIntegerConstant (ctx, env, span, value, ty)
  | toFExp (ctx, env, U.SConExp (span, Syntax.WordConstant value, ty)) = cookWordConstant (ctx, env, span, value, ty)
  | toFExp (ctx, env, U.SConExp (span, Syntax.RealConstant value, ty)) = F.PrimExp (F.RealConstOp value, vector [toFTy (ctx, env, ty)], vector [])
  | toFExp (ctx, env, U.SConExp (span, Syntax.StringConstant value, ty)) = cookStringConstant (ctx, env, span, value, ty)
  | toFExp (ctx, env, U.SConExp (span, Syntax.CharacterConstant value, ty)) = cookCharacterConstant (ctx, env, span, value, ty)
  | toFExp (ctx, env, U.VarExp (span, longvid as TypedSyntax.MkShortVId vid, _, [(tyarg, cts)]))
    = (case TypedSyntax.VIdMap.find (overloads, vid) of
           SOME key => (case tyarg of
                            U.TyCon (_, [], tycon) => (case TypedSyntax.TyNameMap.find (#overloadMap env, tycon) of
                                                           SOME m => (case Syntax.OverloadKeyMap.find (m, key) of
                                                                          SOME exp => exp
                                                                        | NONE => raise Fail ("invalid use of " ^ TypedSyntax.print_VId vid)
                                                                     )
                                                         | NONE => raise Fail ("invalid use of " ^ TypedSyntax.print_VId vid)
                                                      )
                          | _ => raise Fail ("invalid use of " ^ TypedSyntax.print_VId vid)
                       )
         | NONE => if List.exists (fn TypedSyntax.IsEqType _ => true | _ => false) cts then
                       F.AppExp(F.TyAppExp(F.LongVarExp(longvid), toFTy(ctx, env, tyarg)), getEquality(ctx, env, tyarg))
                   else
                       F.TyAppExp(F.LongVarExp(longvid), toFTy(ctx, env, tyarg))
      )
  | toFExp(ctx, env, U.VarExp(span, longvid, _, tyargs))
    = List.foldl (fn ((ty, cts), e) =>
                     if List.exists (fn TypedSyntax.IsEqType _ => true | _ => false) cts then
                         F.AppExp(F.TyAppExp(e, toFTy(ctx, env, ty)), getEquality(ctx, env, ty))
                     else
                         F.TyAppExp(e, toFTy(ctx, env, ty))
                 ) (F.LongVarExp(longvid)) tyargs
  | toFExp(ctx, env, U.RecordExp(span, fields)) = let fun doField (label, e) = (label, toFExp(ctx, env, e))
                                                  in F.RecordExp (List.map doField fields)
                                                  end
  | toFExp(ctx, env, U.RecordExtExp { sourceSpan, fields, baseExp, baseTy as U.RecordType (_, baseFields) })
    = let fun vidForLabel (Syntax.IdentifierLabel x) = x
            | vidForLabel (Syntax.NumericLabel n) = "field" ^ Int.toString n
          fun doField ((label, e), (decs, fields))
              = let val vid = freshVId (ctx, vidForLabel label)
                    val e = toFExp (ctx, env, e)
                    val dec = F.ValDec (vid, NONE, e)
                in (dec :: decs, (label, F.VarExp vid) :: fields)
                end
          val (decs, fields) = List.foldr doField ([], []) fields
          val baseVId = freshVId (ctx, "base")
          val baseDec = F.ValDec (baseVId, SOME (toFTy (ctx, env, baseTy)), toFExp (ctx, env, baseExp))
          val baseExp = F.VarExp baseVId
          val baseFields = Syntax.LabelMap.foldri (fn (label, _, fields) =>
                                                      (label, F.ProjectionExp { label = label, record = baseExp }) :: fields
                                                  ) [] baseFields
      in List.foldr F.LetExp (F.LetExp (baseDec, F.RecordExp (fields @ baseFields))) decs
      end
  | toFExp(ctx, env, U.RecordExtExp { sourceSpan, fields, baseExp, baseTy }) = raise Fail ("record extension of non-record type: " ^ U.print_Ty baseTy)
  | toFExp(ctx, env, U.LetInExp(span, decs, e))
    = let val (env, decs) = toFDecs(ctx, env, decs)
      in List.foldr F.LetExp (toFExp(ctx, env, e)) decs
      end
  | toFExp(ctx, env, U.AppExp(span, U.ProjectionExp { label, ... }, e2)) = F.ProjectionExp { label = label, record = toFExp(ctx, env, e2) }
  | toFExp(ctx, env, U.AppExp(span, e1, e2)) = F.AppExp(toFExp(ctx, env, e1), toFExp(ctx, env, e2))
  | toFExp(ctx, env, U.TypedExp(span, exp, _)) = toFExp(ctx, env, exp)
  | toFExp(ctx, env, U.IfThenElseExp(span, e1, e2, e3)) = F.IfThenElseExp(toFExp(ctx, env, e1), toFExp(ctx, env, e2), toFExp(ctx, env, e3))
  | toFExp(ctx, env, U.CaseExp(span, e, ty, matches))
    = let fun doMatch(pat, exp) = let val (_, pat') = toFPat(ctx, env, pat)
                                  in (pat', toFExp(ctx, env, exp)) (* TODO: environment *)
                                  end
      in F.CaseExp(span, toFExp(ctx, env, e), toFTy(ctx, env, ty), List.map doMatch matches)
      end
  | toFExp(ctx, env, U.FnExp(span, vid, ty, body))
    = let val env' = env (* TODO *)
      in F.FnExp(vid, toFTy(ctx, env, ty), toFExp(ctx, env', body))
      end
  | toFExp(ctx, env, U.ProjectionExp { sourceSpan = span, label, recordTy, fieldTy = _ })
    = let val vid = freshVId(ctx, "tmp")
      in F.FnExp(vid, toFTy(ctx, env, recordTy), F.ProjectionExp { label = label, record = F.VarExp vid })
      end
  | toFExp(ctx, env, U.HandleExp(span, exp, matches))
    = let val exnName = freshVId(ctx, "exn")
          val exnTy = F.TyVar(F.tyNameToTyVar(Typing.primTyName_exn))
          fun doMatch(pat, exp) = let val (_, pat') = toFPat(ctx, env, pat)
                                  in (pat', toFExp(ctx, env, exp)) (* TODO: environment *)
                                  end
          fun isExhaustive (F.WildcardPat _) = true
            | isExhaustive (F.SConPat _) = false
            | isExhaustive (F.VarPat _) = true
            | isExhaustive (F.RecordPat _) = false (* exn is not a record *)
            | isExhaustive (F.ConPat _) = false (* exn is open *)
            | isExhaustive (F.LayeredPat (_, _, _, pat)) = isExhaustive pat
            | isExhaustive (F.VectorPat _) = false (* exn is not a vector *)
          val matches' = List.map doMatch matches
          val matches'' = if List.exists (fn (pat, _) => isExhaustive pat) matches' then
                              matches'
                          else
                              matches' @ [(F.WildcardPat span, F.RaiseExp((* re-throw *) SourcePos.nullSpan, (* TODO: type of raise *) F.RecordType Syntax.LabelMap.empty, F.VarExp(exnName)))]
      in F.HandleExp { body = toFExp(ctx, env, exp)
                     , exnName = exnName
                     , handler = F.CaseExp(span, F.VarExp(exnName), exnTy, matches'')
                     }
      end
  | toFExp(ctx, env, U.RaiseExp(span, ty, exp)) = F.RaiseExp(span, toFTy(ctx, env, ty), toFExp(ctx, env, exp))
  | toFExp(ctx, env, U.ListExp(span, xs, ty)) = F.ListExp(Vector.map (fn x => toFExp(ctx, env, x)) xs, toFTy(ctx, env, ty))
  | toFExp(ctx, env, U.VectorExp(span, xs, ty)) = F.VectorExp(Vector.map (fn x => toFExp(ctx, env, x)) xs, toFTy(ctx, env, ty))
  | toFExp (ctx, env, U.PrimExp (span, Primitives.PrimOp_EQUAL, tyargs, args)) = if Vector.length tyargs = 1 andalso Vector.length args = 2 then
                                                                                     let val tyarg = Vector.sub (tyargs, 0)
                                                                                         val x = toFExp (ctx, env, Vector.sub (args, 0))
                                                                                         val y = toFExp (ctx, env, Vector.sub (args, 1))
                                                                                     in F.AppExp (getEquality (ctx, env, tyarg), F.TupleExp [x, y])
                                                                                     end
                                                                                 else
                                                                                     raise Fail ("invalid arguments to primop '=' (" ^ Int.toString (Vector.length tyargs) ^ ", " ^ Int.toString (Vector.length args) ^ ")")
  | toFExp(ctx, env, U.PrimExp(span, primOp, tyargs, args)) = F.PrimExp(F.PrimFnOp primOp, Vector.map (fn ty => toFTy(ctx, env, ty)) tyargs, Vector.map (fn x => toFExp(ctx, env, x)) args)
and doValBind ctx env (U.TupleBind (span, vars, exp))
    = let val tupleVId = freshVId (ctx, "tmp")
          val exp = toFExp (ctx, env, exp)
          val vars = List.map (fn (vid, ty) => (vid, toFTy (ctx, env, ty))) vars
          val tupleTy = F.TupleType (List.map #2 vars)
          val decs = let fun go (i, []) = []
                           | go (i, (vid, ty) :: xs) = F.ValDec (vid, SOME ty, F.ProjectionExp { label = Syntax.NumericLabel i, record = F.VarExp tupleVId }) :: go (i + 1, xs)
                     in go (1, vars)
                     end
      in F.ValDec (tupleVId, SOME tupleTy, exp) :: decs
      end
  | doValBind ctx env (U.PolyVarBind (span, vid, U.TypeScheme(tvs, ty), exp))
    = let val ty0 = toFTy (ctx, env, ty)
          val ty' = List.foldr (fn ((tv,cts),ty1) =>
                                   case cts of
                                       [] => F.ForallType (tv, F.TypeKind, ty1)
                                     | [U.IsEqType _] => F.ForallType (tv, F.TypeKind, F.FnType (F.EqualityType (F.TyVar tv), ty1))
                                     | _ => raise Fail "invalid type constraint"
                               ) ty0 tvs
          fun doExp (env', [])
              = toFExp(ctx, env', exp)
            | doExp (env', (tv,cts) :: rest)
              = (case cts of
                     [] => F.TyAbsExp (tv, F.TypeKind, doExp (env', rest))
                   | [U.IsEqType _] => let val vid = freshVId(ctx, "eq")
                                           val env'' = updateEqualityForTyVarMap (fn m => TypedSyntax.TyVarMap.insert (m, tv, vid), env')
                                       in F.TyAbsExp (tv, F.TypeKind, F.FnExp(vid, F.EqualityType(F.TyVar tv), doExp(env'', rest)))
                                       end
                   | _ => raise Fail "invalid type constraint"
                )
      in [F.ValDec (vid, SOME ty', doExp (env, tvs))]
      end
and doRecValBind ctx env (U.TupleBind (span, vars, exp)) = raise Fail "unexpected TupleBind in RecValDec"
  | doRecValBind ctx env (U.PolyVarBind (span, vid, U.TypeScheme(tvs, ty), exp))
    = let val ty0 = toFTy (ctx, env, ty)
          val ty' = List.foldr (fn ((tv,cts),ty1) =>
                                   case cts of
                                       [] => F.ForallType (tv, F.TypeKind, ty1)
                                     | [U.IsEqType _] => F.ForallType (tv, F.TypeKind, F.FnType (F.EqualityType (F.TyVar tv), ty1))
                                     | _ => raise Fail "invalid type constraint"
                               ) ty0 tvs
          fun doExp (env', [])
              = toFExp(ctx, env', exp)
            | doExp (env', (tv,cts) :: rest)
              = (case cts of
                     [] => F.TyAbsExp (tv, F.TypeKind, doExp (env', rest))
                   | [U.IsEqType _] => let val vid = freshVId(ctx, "eq")
                                           val env'' = updateEqualityForTyVarMap (fn m => TypedSyntax.TyVarMap.insert (m, tv, vid), env')
                                       in F.TyAbsExp (tv, F.TypeKind, F.FnExp(vid, F.EqualityType(F.TyVar tv), doExp(env'', rest)))
                                       end
                   | _ => raise Fail "invalid type constraint"
                )
      in (vid, ty', doExp(env, tvs))
      end
and typeSchemeToTy (ctx, env, TypedSyntax.TypeScheme (vars, ty))
    = let fun go env [] = toFTy(ctx, env, ty)
            | go env ((tv, []) :: xs) = let val env' = env (* TODO *)
                                        in F.ForallType(tv, F.TypeKind, go env' xs)
                                        end
            | go env ((tv, [U.IsEqType _]) :: xs) = let val env' = env (* TODO *)
                                                    in F.ForallType(tv, F.TypeKind, F.FnType(F.EqualityType(F.TyVar tv), go env' xs))
                                                    end
            | go env ((tv, _) :: xs) = raise Fail "invalid type constraint"
      in go env vars
      end
and getEquality (ctx, env, U.TyCon (span, tyargs, tyname))
    = (case TypedSyntax.TyNameMap.find (#equalityForTyNameMap env, tyname) of
           NONE => raise Fail (TypedSyntax.PrettyPrint.print_TyName tyname ^ " does not admit equality")
         | SOME longvid => let val typesApplied = List.foldl (fn (tyarg, exp) => F.TyAppExp (exp, toFTy (ctx, env, tyarg))) (F.LongVarExp longvid) tyargs
                           in if Typing.isRefOrArray tyname then
                                  typesApplied
                              else
                                  List.foldl (fn (tyarg, exp) => F.AppExp (exp, getEquality (ctx, env, tyarg))) typesApplied tyargs
                           end
      )
  | getEquality (ctx, env, U.TyVar (span, tv)) = (case TypedSyntax.TyVarMap.find (#equalityForTyVarMap env, tv) of
                                                      NONE => raise Fail ("equality for the type variable not found: " ^ TypedSyntax.PrettyPrint.print_TyVar tv)
                                                    | SOME vid => F.VarExp vid
                                                 )
  | getEquality (ctx, env, U.RecordType(span, fields)) = F.RecordEqualityExp (Syntax.LabelMap.foldli (fn (label, ty, xs) => (label, getEquality(ctx, env, ty)) :: xs) [] fields)
  | getEquality (ctx, env, U.FnType _) = raise Fail "functions are not equatable; this should have been a type error"
and toFDecs(ctx, env, []) = (env, [])
  | toFDecs(ctx, env, U.ValDec(span, valbinds) :: decs)
    = let val dec = List.foldr (fn (valbind, decs) => doValBind ctx env valbind @ decs) [] valbinds
          val (env, decs) = toFDecs (ctx, env, decs)
      in (env, dec @ decs)
      end
  | toFDecs(ctx, env, U.RecValDec(span, valbinds) :: decs)
    = let val dec = F.RecValDec (List.map (doRecValBind ctx env) valbinds)
          val (env, decs) = toFDecs (ctx, env, decs)
      in (env, dec :: decs)
      end
  | toFDecs(ctx, env, U.TypeDec(span, typbinds) :: decs) = toFDecs(ctx, env, decs)
  | toFDecs(ctx, env, U.DatatypeDec(span, datbinds) :: decs)
    = let val dec = F.DatatypeDec (List.map (fn datbind => doDatBind(ctx, env, datbind)) datbinds)
          val (env, valbinds) = genEqualitiesForDatatypes(ctx, env, datbinds)
          val (env, decs) = toFDecs(ctx, env, decs)
      in (env, dec :: (if List.null valbinds then decs else F.RecValDec valbinds :: decs))
      end
  | toFDecs(ctx, env as { exnTagMap, ... }, U.ExceptionDec(span, exbinds) :: decs)
    = let val (exnTagMap, exbinds) = List.foldr (fn (U.ExBind (span, vid as TypedSyntax.MkVId (name, _), optTy), (exnTagMap, xs)) =>
                                                    let val tag = freshVId(ctx, name ^ "_tag")
                                                    in ( TypedSyntax.LongVIdMap.insert (exnTagMap, U.MkShortVId vid, F.Root tag)
                                                       , F.ExceptionDec { conName = vid
                                                                        , tagName = tag
                                                                        , payloadTy = Option.map (fn ty => toFTy(ctx, env, ty)) optTy
                                                                        } :: xs
                                                       )
                                                    end
                                                | (U.ExReplication(span, vid, longvid, optTy), (exnTagMap, xs)) =>
                                                  (case TypedSyntax.LongVIdMap.find (#exnTagMap env, longvid) of
                                                       SOME tagPath => ( TypedSyntax.LongVIdMap.insert (exnTagMap, U.MkShortVId vid, tagPath)
                                                                       , F.ExceptionRepDec { conName = vid
                                                                                           , conPath = LongVIdToPath longvid
                                                                                           , tagPath = tagPath
                                                                                           , payloadTy = Option.map (fn ty => toFTy(ctx, env, ty)) optTy
                                                                                           } :: xs
                                                                       )
                                                     | NONE => emitError (ctx, [span], "exception not found: " ^ TypedSyntax.print_LongVId longvid)
                                                  )
                                                ) (exnTagMap, []) exbinds
          val env = updateExnTagMap (fn _ => exnTagMap, env)
          val (env, decs) = toFDecs(ctx, env, decs)
      in (env, exbinds @ decs)
      end
  | toFDecs(ctx, env, U.GroupDec(span, decs) :: decs') = let val (env, decs) = toFDecs(ctx, env, decs)
                                                             val (env, decs') = toFDecs(ctx, env, decs')
                                                         in (env, case decs of
                                                                      [] => decs'
                                                                    | [dec] => dec :: decs'
                                                                    | _ => F.GroupDec(NONE, decs) :: decs'
                                                            )
                                                         end
  | toFDecs(ctx, env, U.OverloadDec(span, class, tyname, map) :: decs) = let val map = Syntax.OverloadKeyMap.map (fn exp => toFExp(ctx, env, exp)) map
                                                                             val env = updateOverloadMap (fn m => TypedSyntax.TyNameMap.insert (m, tyname, map), env)
                                                                             val (env, decs) = toFDecs(ctx, env, decs)
                                                                         in (env, decs)
                                                                         end
  | toFDecs (ctx, env, U.EqualityDec (span, tyvars, tyname, exp) :: decs) = let val vid = freshVId (ctx, "eq")
                                                                                val tyvarEqualities = if Typing.isRefOrArray tyname then
                                                                                                          []
                                                                                                      else
                                                                                                          List.map (fn tv => (tv, freshVId (ctx, "eq"))) tyvars
                                                                                val env = updateEqualityForTyNameMap (fn m => TypedSyntax.TyNameMap.insert (m, tyname, TypedSyntax.MkShortVId vid), env)
                                                                                val innerEnv = updateEqualityForTyVarMap (fn m => List.foldl TypedSyntax.TyVarMap.insert' m tyvarEqualities, env)
                                                                                val exp = toFExp (ctx, innerEnv, exp)
                                                                                val exp = List.foldr (fn ((tv, eqParam), exp) => F.FnExp (eqParam, F.EqualityType (F.TyVar tv), exp)) exp tyvarEqualities
                                                                                val exp = List.foldr (fn (tv, exp) => F.TyAbsExp (tv, F.TypeKind, exp)) exp tyvars
                                                                                val ty = F.EqualityType (F.TyVar (F.tyNameToTyVar tyname))
                                                                                val ty = List.foldr (fn ((tv, _), ty) => F.FnType (F.EqualityType (F.TyVar tv), ty)) ty tyvarEqualities
                                                                                val ty = List.foldr (fn (tv, ty) => F.ForallType (tv, F.TypeKind, ty)) ty tyvars
                                                                                val dec = F.RecValDec [(vid, ty, exp)]
                                                                                val (env, decs) = toFDecs (ctx, env, decs)
                                                                            in (env, dec :: decs)
                                                                            end
and doDatBind(ctx, env, U.DatBind(span, tyvars, tycon, conbinds, _)) = F.DatBind(tyvars, F.tyNameToTyVar tycon, List.map (fn conbind => doConBind(ctx, env, conbind)) conbinds)
and doConBind(ctx, env, U.ConBind(span, vid, NONE)) = F.ConBind(vid, NONE)
  | doConBind(ctx, env, U.ConBind(span, vid, SOME ty)) = F.ConBind(vid, SOME (toFTy(ctx, env, ty)))
and genEqualitiesForDatatypes (ctx, env, datbinds) : Env * (TypedSyntax.VId * F.Ty * F.Exp) list
    = let val nameMap = List.foldl (fn (U.DatBind (span, tyvars, tycon as TypedSyntax.MkTyName (name, _), conbinds, true), map) => TypedSyntax.TyNameMap.insert (map, tycon, freshVId (ctx, "EQUAL" ^ name))
                                   | (_, map) => map) TypedSyntax.TyNameMap.empty datbinds
          val env' = updateEqualityForTyNameMap (fn m => TypedSyntax.TyNameMap.unionWith #2 (#equalityForTyNameMap env, TypedSyntax.TyNameMap.map U.MkShortVId nameMap), env)
          fun doDatBind(U.DatBind(span, tyvars, tyname, conbinds, true), valbinds)
              = let val vid = TypedSyntax.TyNameMap.lookup (nameMap, tyname)
                    val tyvars'' = List.map F.TyVar tyvars
                    val ty = List.foldr (fn (tv, ty) => F.FnType(F.EqualityType (F.TyVar tv), ty)) (F.EqualityType (F.TyCon(tyvars'', tyname))) tyvars
                    val ty = List.foldr (fn (tv, ty) => F.ForallType(tv, F.TypeKind, ty)) ty tyvars
                    val tyvars' = List.map (fn tv => (tv, freshVId(ctx, "eq"))) tyvars
                    val eqForTyVars = List.foldl TypedSyntax.TyVarMap.insert' TypedSyntax.TyVarMap.empty tyvars'
                    val env'' = updateEqualityForTyVarMap (fn m => TypedSyntax.TyVarMap.unionWith #2 (m, eqForTyVars), env')
                    val body = let val param = freshVId(ctx, "p")
                                   val paramTy = let val ty = F.TyCon(tyvars'', tyname)
                                                 in F.PairType(ty, ty)
                                                 end
                               in F.FnExp ( param
                                          , paramTy
                                          , F.CaseExp ( span
                                                      , F.VarExp(param)
                                                      , paramTy
                                                      , List.foldr (fn (U.ConBind (span, conName, NONE), rest) =>
                                                                       let val conPat = F.ConPat(span, F.Root(conName), NONE, tyvars'')
                                                                       in ( F.TuplePat (span, [conPat, conPat])
                                                                          , F.VarExp(InitialEnv.VId_true)
                                                                          ) :: rest
                                                                       end
                                                                   | (U.ConBind (span, conName, SOME payloadTy), rest) =>
                                                                     let val payload1 = freshVId(ctx, "a")
                                                                         val payload2 = freshVId(ctx, "b")
                                                                         val payloadEq = getEquality(ctx, env'', payloadTy)
                                                                         val payloadTy = toFTy(ctx, env, payloadTy)
                                                                     in ( F.TuplePat (span, [F.ConPat(span, F.Root(conName), SOME (F.VarPat(span, payload1, payloadTy)), tyvars''), F.ConPat(span, F.Root(conName), SOME (F.VarPat(span, payload2, payloadTy)), tyvars'')])
                                                                        , F.AppExp(payloadEq, F.TupleExp [F.VarExp(payload1), F.VarExp(payload2)])
                                                                        ) :: rest
                                                                     end
                                                                   )
                                                                   [(F.WildcardPat span, F.VarExp(InitialEnv.VId_false))]
                                                                   conbinds
                                                      )
                                          )
                               end
                    val body = List.foldr (fn ((tv, eqParam), body) => F.FnExp ( eqParam
                                                                               , F.EqualityType (F.TyVar tv)
                                                                               , body
                                                                               )
                                          ) body tyvars'
                    val body = List.foldr (fn (tv, body) => F.TyAbsExp(tv, F.TypeKind, body)) body tyvars
                in (vid, ty, body) :: valbinds
                end
            | doDatBind(_, valbinds) = valbinds
      in (env', List.foldr doDatBind [] datbinds)
      end
fun signatureToTy(ctx, env, { valMap, tyConMap, strMap } : U.Signature)
    = let val exnTags = Syntax.VIdMap.foldli (fn (vid, (tysc, ids), set) => if ids = Syntax.ExceptionConstructor then
                                                                                Syntax.VIdSet.add(set, vid)
                                                                            else
                                                                                set
                                             ) Syntax.VIdSet.empty valMap
      in F.SigType { valMap = Syntax.VIdMap.map (fn (tysc, ids) => (typeSchemeToTy (ctx, env, tysc), ids)) valMap
                   , strMap = Syntax.StrIdMap.map (fn U.MkSignature s => signatureToTy(ctx, env, s)) strMap
                   , exnTags = exnTags
                   }
      end
fun getEqualityForTypeFunction (ctx, env, U.TypeFunction (tyvars, ty))
    = let val tyvars' = List.map (fn tv => (tv, freshVId (ctx, "eq"))) tyvars
          val equalityEnv = updateEqualityForTyVarMap (fn m => List.foldl TypedSyntax.TyVarMap.insert' m tyvars', env)
          val equality = getEquality (ctx, equalityEnv, ty)
          val equality = List.foldr (fn ((tv, eqParam), body) => F.FnExp (eqParam, F.EqualityType (F.TyVar tv), body)) equality tyvars'
          val equality = List.foldr (fn (tv, body) => F.TyAbsExp (tv, F.TypeKind, body)) equality tyvars
      in equality
      end
fun strExpToFExp(ctx, env : Env, U.StructExp { sourceSpan, valMap, tyConMap, strMap }) : Env * F.Dec list * F.Exp
    = let val exp = F.StructExp { valMap = Syntax.VIdMap.map (fn (longvid, ids) => LongVIdToPath(longvid)) valMap
                                , strMap = Syntax.StrIdMap.map LongStrIdToPath strMap
                                , exnTagMap = Syntax.VIdMap.mapPartial (fn (longvid, ids) => if ids = Syntax.ExceptionConstructor then
                                                                                                 case TypedSyntax.LongVIdMap.find (#exnTagMap env, longvid) of
                                                                                                     SOME path => SOME path
                                                                                                   | NONE => raise Fail ("exception tag not found for " ^ TypedSyntax.print_LongVId longvid)
                                                                                             else
                                                                                                 NONE
                                                                       ) valMap
                                }
      in (env, [], exp)
      end
  | strExpToFExp(ctx, env, U.StrIdExp(span, longstrid)) = (env, [], LongStrIdExp longstrid)
  | strExpToFExp(ctx, env, U.PackedStrExp { sourceSpan, strExp, payloadTypes, packageSig })
    = let val (env', decs, exp) = strExpToFExp(ctx, env, strExp)
          val packageTy = signatureToTy (ctx, env, #s packageSig)
          fun EqualityTyForArity 0 xs t = List.foldl F.FnType (F.EqualityType t) xs
            | EqualityTyForArity n xs t = let val tv = freshTyVar ctx
                                          in F.ForallType (tv, F.TypeKind, EqualityTyForArity (n - 1) (F.EqualityType (F.TyVar tv) :: xs) (F.AppType { applied = t, arg = F.TyVar tv }))
                                          end
          val (exp, packageTy) = ListPair.foldrEq (fn (typeFunction as U.TypeFunction (tyvars, payloadTy), { tyname, arity, admitsEquality }, (exp, packageTy)) =>
                                                      let val kind = F.arityToKind arity
                                                          val payloadTy' = List.foldr (fn (tv, ty) => F.TypeFn (tv, F.TypeKind, ty)) (toFTy (ctx, env', payloadTy)) tyvars
                                                      in if admitsEquality then
                                                             let val packageTy = F.ExistsType (F.tyNameToTyVar tyname, kind, F.PairType(EqualityTyForArity arity [] (F.TyVar (F.tyNameToTyVar tyname)), packageTy)) (* exists 'a. ('a * 'a -> bool) * packageTy / exists t. (forall 'a. forall 'b. ... ('a * 'a -> bool) -> ('b * 'b -> bool) -> ... -> (t 'a 'b ... * t 'a 'b ... -> bool)) * packageTy *)
                                                                 val equality = getEqualityForTypeFunction (ctx, env', typeFunction)
                                                             in (F.PackExp { payloadTy = payloadTy', exp = F.TupleExp [equality, exp], packageTy = packageTy }, packageTy)
                                                             end
                                                         else
                                                             let val packageTy = F.ExistsType (F.tyNameToTyVar tyname, kind, packageTy)
                                                             in (F.PackExp { payloadTy = payloadTy', exp = exp, packageTy = packageTy }, packageTy)
                                                             end
                                                      end
                                                  ) (exp, packageTy) (payloadTypes, #bound packageSig)
      in (env', decs, exp)
      end
  | strExpToFExp(ctx, env, U.FunctorAppExp { sourceSpan, funId, argumentTypes, argumentStr, packageSig })
    = let val (env', decs, argumentStr) = strExpToFExp (ctx, env, argumentStr)
          (* val packageTy = signatureToTy (ctx, env, #s packageSig) *)
          (* <funid> <argument type>... <argument type's equality>... <structure> *)
          val exp = F.VarExp (case funId of U.MkFunId (name, n) => U.MkVId (name, n)) (* the functor id *)
          val exp = List.foldl (fn ({ typeFunction = U.TypeFunction (tyvars, ty), admitsEquality = _ }, exp) =>
                                   let val ty = List.foldr (fn (tv, ty) => F.TypeFn (tv, F.TypeKind, ty)) (toFTy (ctx, env', ty)) tyvars
                                   in F.TyAppExp (exp, ty)
                                   end
                               ) exp argumentTypes (* apply the types *)
          val exp = List.foldl (fn ({ typeFunction, admitsEquality = true }, exp) => F.AppExp (exp, getEqualityForTypeFunction (ctx, env, typeFunction))
                               | ({ typeFunction = _, admitsEquality = false }, exp) => exp
                               ) exp argumentTypes (* apply the equalities *)
          val exp = F.AppExp (exp, argumentStr) (* apply the structure *)
      in (env (* What to do? *), decs, exp)
      end
  | strExpToFExp(ctx, env, U.LetInStrExp(span, strdecs, strexp)) = let val (env', decs) = strDecsToFDecs(ctx, env, strdecs)
                                                                       val (env', decs', exp) = strExpToFExp(ctx, env', strexp)
                                                                   in (env', decs @ decs', exp)
                                                                   end
and strDecToFDecs(ctx, env : Env, U.CoreDec(span, dec)) = toFDecs(ctx, env, [dec])
  | strDecToFDecs(ctx, env, U.StrBindDec(span, strid, strexp, { s, bound }))
    = let val vid = F.strIdToVId strid
          val ty = signatureToTy (ctx, env, s)
          val (env', decs0, exp) = strExpToFExp(ctx, env, strexp)
          fun doExnTagMap (strids, { valMap, strMap, ... }, path) exnTagMap
              = let val exnTagMap = Syntax.VIdMap.foldli (fn (vid, (tysc, Syntax.ExceptionConstructor), m) => TypedSyntax.LongVIdMap.insert (m, U.MkLongVId (strid, strids, vid), F.Child (path, F.ExnTagLabel vid))
                                                         | (_, (_, _), m) => m
                                                         ) exnTagMap valMap
                in Syntax.StrIdMap.foldli (fn (strid, U.MkSignature s, m) => doExnTagMap (strids @ [strid], s, F.Child(path, F.StructLabel strid)) m) exnTagMap strMap
                end
          val env'' = updateEqualityForTyNameMap ( fn m => TypedSyntax.TyNameMap.unionWith #2 (m, #equalityForTyNameMap env')
                                                 , updateExnTagMap (doExnTagMap ([], s, F.Root vid), env)
                                                 )
          val (decs, exp, env) = List.foldl (fn ({ tyname, arity, admitsEquality }, (decs, exp, env)) =>
                                                let val vid = freshVId(ctx, case vid of U.MkVId(name,_) => name)
                                                in if admitsEquality then
                                                       let val equalityVId = freshVId(ctx, "eq")
                                                           val strVId = freshVId(ctx, case vid of U.MkVId(name,_) => name)
                                                       in ( F.ValDec (equalityVId, NONE, F.ProjectionExp { label = Syntax.NumericLabel 1, record = F.VarExp vid })
                                                            :: F.ValDec (strVId, NONE, F.ProjectionExp { label = Syntax.NumericLabel 2, record = F.VarExp vid })
                                                            :: F.UnpackDec (F.tyNameToTyVar tyname, F.arityToKind arity, vid, (* TODO *) F.RecordType Syntax.LabelMap.empty, exp)
                                                            :: decs
                                                          , F.VarExp strVId
                                                          , updateEqualityForTyNameMap (fn m => U.TyNameMap.insert (m, tyname (* case tyname of U.NamedTyVar(name,_,n) => U.MkTyName(name,n) | U.AnonymousTyVar(n) => U.MkTyName("",n) *), U.MkShortVId equalityVId), env)
                                                          )
                                                       end
                                                   else
                                                       (F.UnpackDec (F.tyNameToTyVar tyname, F.arityToKind arity, vid, (* TODO *) F.RecordType Syntax.LabelMap.empty, exp) :: decs, F.VarExp vid, env)
                                                end
                                            ) ([], exp, env'') bound
      in (env, [F.GroupDec(NONE, decs0 @ List.rev (F.ValDec (vid, SOME ty, exp) :: decs))])
      end
  | strDecToFDecs(ctx, env, U.GroupStrDec(span, decs)) = let val (env, decs) = strDecsToFDecs(ctx, env, decs)
                                                         in (env, case decs of
                                                                      [] => decs
                                                                    | [_] => decs
                                                                    | _ => [F.GroupDec(NONE, decs)]
                                                            )
                                                         end
and strDecsToFDecs(ctx, env : Env, []) = (env, [])
  | strDecsToFDecs(ctx, env, dec :: decs) = let val (env, dec) = strDecToFDecs(ctx, env, dec)
                                                val (env, decs) = strDecsToFDecs(ctx, env, decs)
                                            in (env, dec @ decs)
                                            end
fun funDecToFDec(ctx, env, (funid, (types, paramStrId, paramSig, bodyStr))) : F.Dec
    = let val funid = case funid of U.MkFunId (name, n) => U.MkVId (name, n)
          val paramId = case paramStrId of U.MkStrId (name, n) => U.MkVId (name, n)
          val (equalityForTyNameMap, equalityVars)
              = List.foldr (fn ({ tyname, arity, admitsEquality = true }, (m, xs)) => let val vid = freshVId(ctx, "eq")
                                                                                      in (U.TyNameMap.insert (m, tyname, U.MkShortVId vid), (tyname, arity, vid) :: xs)
                                                                                      end
                           | ({ admitsEquality = false, ... }, acc) => acc
                           ) (#equalityForTyNameMap env, []) types
          val env' = updateEqualityForTyNameMap (fn _ => equalityForTyNameMap, env)
          fun doExnTag (s : U.Signature) exnTagMap = let val exnTagMap = Syntax.VIdMap.foldli (fn (vid, (_, Syntax.ExceptionConstructor), m) => TypedSyntax.LongVIdMap.insert (m, TypedSyntax.MkLongVId (paramStrId, [], vid), (F.Child (F.Root paramId, F.ExnTagLabel vid)))
                                                                                              | (_, (_, _), m) => m
                                                                                              ) exnTagMap (#valMap s)
                                                     in Syntax.StrIdMap.foldli (fn (strid, U.MkSignature s', m) => doExnTagInStr ([strid], F.Child (F.Root paramId, F.StructLabel strid), s') m) exnTagMap (#strMap s)
                                                     end
          and doExnTagInStr (q, path, s : U.Signature) exnTagMap = let val exnTagMap = Syntax.VIdMap.foldli (fn (vid, (_, Syntax.ExceptionConstructor), m) => TypedSyntax.LongVIdMap.insert (m, TypedSyntax.MkLongVId (paramStrId, q, vid), (F.Child (path, F.ExnTagLabel vid)))
                                                                                                            | (_, (_, _), m) => m
                                                                                                            ) exnTagMap (#valMap s)
                                                                   in Syntax.StrIdMap.foldli (fn (strid, U.MkSignature s', m) => doExnTagInStr (q @ [strid], F.Child (path, F.StructLabel strid), s') m) exnTagMap (#strMap s)
                                                                   end
          val env' = updateExnTagMap (doExnTag paramSig, env')
          val (_, bodyDecs, bodyExp) = strExpToFExp (ctx, env', bodyStr)
          val funexp = F.FnExp (paramId, signatureToTy (ctx, env, paramSig), List.foldr F.LetExp bodyExp bodyDecs)
          val funexp = List.foldr (fn ((tyname, arity, vid), funexp) => F.FnExp (vid, F.EqualityType (F.TyVar (F.tyNameToTyVar tyname)), funexp)) funexp equalityVars (* equalities *)
          val funexp = List.foldr (fn ({ tyname, arity, admitsEquality = _ }, funexp) => F.TyAbsExp (F.tyNameToTyVar tyname, F.arityToKind arity, funexp)) funexp types (* type parameters *)
      in F.ValDec (funid, NONE, funexp)
      end
fun programToFDecs(ctx, env : Env, []) = (env, [])
  | programToFDecs (ctx, env, TypedSyntax.StrDec dec :: topdecs) = let val (env, decs) = strDecToFDecs (ctx, env, dec)
                                                                       val (env, decs') = programToFDecs (ctx, env, topdecs)
                                                                   in (env, decs @ decs')
                                                                   end
  | programToFDecs (ctx, env, TypedSyntax.FunDec dec :: topdecs) = let val dec = funDecToFDec (ctx, env, dec)
                                                                       val (env, decs) = programToFDecs (ctx, env, topdecs)
                                                                   in (env, dec :: decs)
                                                                   end
fun isAlphaNumName name = List.all (fn c => Char.isAlphaNum c orelse c = #"_") (String.explode name)
fun addExport(ctx, tenv: Typing.Env, decs)
    = case (Syntax.VIdMap.find (#valMap tenv, Syntax.MkVId "export"), Syntax.StrIdMap.find (#strMap tenv, Syntax.MkStrId "export")) of
          (NONE, NONE) => raise Fail "No value to export was found."
        | (SOME (_, _, longvid), NONE) => decs @ [ F.ExportValue (F.LongVarExp longvid) ]
        | (NONE, SOME ({ valMap, ... }, U.MkLongStrId(strid0, strids))) =>
          let val fields = Syntax.VIdMap.listItems (Syntax.VIdMap.mapPartiali (fn (vid, _) => let val name = Syntax.getVIdName vid
                                                                                              in if isAlphaNumName name then
                                                                                                     SOME (name, F.LongVarExp(U.MkLongVId(strid0, strids, vid)))
                                                                                                 else if String.isSuffix "'" name then
                                                                                                     let val name' = String.substring (name, 0, String.size name - 1)
                                                                                                     in if isAlphaNumName name' andalso not (Syntax.VIdMap.inDomain (valMap, Syntax.MkVId name')) then
                                                                                                            SOME (name', F.LongVarExp(U.MkLongVId(strid0, strids, vid)))
                                                                                                        else
                                                                                                            NONE
                                                                                                     end
                                                                                                 else
                                                                                                     NONE
                                                                                              end
                                                                              ) valMap)
          in decs @ [ F.ExportModule (Vector.fromList fields) ]
          end
        | (SOME _, SOME _) => raise Fail "The value to export is ambiguous."
end (* local *)
end (* structure ToFSyntax *)
