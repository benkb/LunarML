(*
 * Copyright (c) 2023 ARATA Mizuki
 * This file is part of LunarML.
 *)
structure PlTransform :>
sig
  type Context = {nextVId: int ref}
  val doProgram: Context -> PlSyntax.Block -> PlSyntax.Block
end =
struct
  structure PL = PlSyntax

  fun collectLetConstStat (PL.LetStat vars) acc =
        Vector.foldl
          (fn ((vid, _), acc) => PL.IdSet.add (acc, PL.UserDefinedId vid)) acc
          vars
    | collectLetConstStat (PL.ConstStat vars) acc =
        Vector.foldl
          (fn ((vid, _), acc) => PL.IdSet.add (acc, PL.UserDefinedId vid)) acc
          vars
    | collectLetConstStat (PL.ExpStat _) acc = acc
    | collectLetConstStat (PL.IfStat _) acc = acc
    | collectLetConstStat (PL.ReturnStat _) acc = acc
    | collectLetConstStat (PL.TryCatchStat _) acc = acc
    | collectLetConstStat (PL.ThrowStat _) acc = acc
    | collectLetConstStat (PL.BlockStat _) acc = acc
    | collectLetConstStat (PL.LoopStat _) acc = acc
    | collectLetConstStat (PL.SwitchStat _) acc = acc
    | collectLetConstStat (PL.BreakStat _) acc = acc
    | collectLetConstStat (PL.ContinueStat _) acc = acc
    | collectLetConstStat (PL.DefaultExportStat _) acc = acc
    | collectLetConstStat (PL.NamedExportStat _) acc = acc
  and collectLetConstBlock stats acc =
    Vector.foldl (fn (stat, acc) => collectLetConstStat stat acc) acc stats

  fun freeVarsExp (_, PL.ConstExp _) acc = acc
    | freeVarsExp (_, PL.ThisExp) acc = acc
    | freeVarsExp (bound, PL.VarExp x) acc =
        if PL.IdSet.member (bound, x) then acc else PL.IdSet.add (acc, x)
    | freeVarsExp (bound, PL.ObjectExp fields) acc =
        Vector.foldl (fn ((_, exp), acc) => freeVarsExp (bound, exp) acc) acc
          fields
    | freeVarsExp (bound, PL.ArrayExp elems) acc =
        Vector.foldl (fn (exp, acc) => freeVarsExp (bound, exp) acc) acc elems
    | freeVarsExp (bound, PL.CallExp (x, ys)) acc =
        Vector.foldl (fn (exp, acc) => freeVarsExp (bound, exp) acc)
          (freeVarsExp (bound, x) acc) ys
    | freeVarsExp (bound, PL.MethodExp (x, _, ys)) acc =
        Vector.foldl (fn (exp, acc) => freeVarsExp (bound, exp) acc)
          (freeVarsExp (bound, x) acc) ys
    | freeVarsExp (bound, PL.NewExp (x, ys)) acc =
        Vector.foldl (fn (exp, acc) => freeVarsExp (bound, exp) acc)
          (freeVarsExp (bound, x) acc) ys
    | freeVarsExp (bound, PL.FunctionExp (params, body)) acc =
        let
          val bound' =
            Vector.foldl (fn (id, bound) => PL.IdSet.add (bound, id)) bound
              params
          val bound'' = collectLetConstBlock body bound'
        in
          freeVarsBlock (bound'', body) acc
        end
    | freeVarsExp (bound, PL.BinExp (_, x, y)) acc =
        freeVarsExp (bound, x) (freeVarsExp (bound, y) acc)
    | freeVarsExp (bound, PL.UnaryExp (_, x)) acc =
        freeVarsExp (bound, x) acc
    | freeVarsExp (bound, PL.IndexExp (x, y)) acc =
        freeVarsExp (bound, x) (freeVarsExp (bound, y) acc)
    | freeVarsExp (bound, PL.CondExp (x, y, z)) acc =
        freeVarsExp (bound, x) (freeVarsExp (bound, y)
          (freeVarsExp (bound, z) acc))
  and freeVarsStat (bound, PL.LetStat vars) acc =
        Vector.foldl
          (fn ((_, NONE), acc) => acc
            | ((_, SOME exp), acc) => freeVarsExp (bound, exp) acc) acc vars
    | freeVarsStat (bound, PL.ConstStat vars) acc =
        Vector.foldl (fn ((_, exp), acc) => freeVarsExp (bound, exp) acc) acc
          vars
    | freeVarsStat (bound, PL.ExpStat exp) acc =
        freeVarsExp (bound, exp) acc
    | freeVarsStat (bound, PL.IfStat (cond, then', else')) acc =
        freeVarsExp (bound, cond) (freeVarsBlock (bound, then')
          (freeVarsBlock (bound, else') acc))
    | freeVarsStat (_, PL.ReturnStat NONE) acc = acc
    | freeVarsStat (bound, PL.ReturnStat (SOME exp)) acc =
        freeVarsExp (bound, exp) acc
    | freeVarsStat (bound, PL.TryCatchStat (try, vid, catch)) acc =
        freeVarsBlock (bound, try)
          (freeVarsBlock (PL.IdSet.add (bound, PL.UserDefinedId vid), catch) acc)
    | freeVarsStat (bound, PL.ThrowStat exp) acc =
        freeVarsExp (bound, exp) acc
    | freeVarsStat (bound, PL.BlockStat (_, block)) acc =
        freeVarsBlock (bound, block) acc
    | freeVarsStat (bound, PL.LoopStat (_, block)) acc =
        freeVarsBlock (bound, block) acc
    | freeVarsStat (bound, PL.SwitchStat (exp, cases)) acc =
        List.foldl (fn ((_, block), acc) => freeVarsBlock (bound, block) acc)
          (freeVarsExp (bound, exp) acc) cases
    | freeVarsStat (_, PL.BreakStat _) acc = acc
    | freeVarsStat (_, PL.ContinueStat _) acc = acc
    | freeVarsStat (bound, PL.DefaultExportStat exp) acc =
        freeVarsExp (bound, exp) acc
    | freeVarsStat (bound, PL.NamedExportStat entities) acc =
        Vector.foldl
          (fn ((x, _), acc) =>
             if PL.IdSet.member (bound, x) then acc else PL.IdSet.add (acc, x))
          acc entities
  and freeVarsBlock (bound, stats) acc =
    let
      val bound' = collectLetConstBlock stats bound
    in
      Vector.foldl (fn (stat, acc) => freeVarsStat (bound', stat) acc) acc stats
    end

  type Context = {nextVId: int ref}

  fun freshVId (ctx: Context, name) =
    let
      val n = !(#nextVId ctx)
      val _ = #nextVId ctx := n + 1
    in
      TypedSyntax.MkVId (name, n)
    end

  fun goExp (_, _, _, e as PL.ConstExp _) = ([], e)
    | goExp (_, _, _, e as PL.ThisExp) = ([], e)
    | goExp (_, _, _, e as PL.VarExp _) = ([], e)
    | goExp (ctx, bound, depth, PL.ObjectExp fields) =
        let
          val (decs, fields') =
            Vector.foldr
              (fn ((key, exp), (decs, fields)) =>
                 let val (decs', exp') = goExp (ctx, bound, depth, exp)
                 in (decs' @ decs, (key, exp') :: fields)
                 end) ([], []) fields
        in
          (decs, PL.ObjectExp (Vector.fromList fields'))
        end
    | goExp (ctx, bound, depth, PL.ArrayExp elems) =
        let val (decs, elems') = goExpVector (ctx, bound, depth, elems)
        in (decs, PL.ArrayExp elems')
        end
    | goExp (ctx, bound, depth, PL.CallExp (x, ys)) =
        let
          val (decs, x') = goExp (ctx, bound, depth, x)
          val (decs', ys') = goExpVector (ctx, bound, depth, ys)
        in
          (decs @ decs', PL.CallExp (x', ys'))
        end
    | goExp (ctx, bound, depth, PL.MethodExp (x, name, ys)) =
        let
          val (decs, x') = goExp (ctx, bound, depth, x)
          val (decs', ys') = goExpVector (ctx, bound, depth, ys)
        in
          (decs @ decs', PL.MethodExp (x', name, ys'))
        end
    | goExp (ctx, bound, depth, PL.NewExp (x, ys)) =
        let
          val (decs, x') = goExp (ctx, bound, depth, x)
          val (decs', ys') = goExpVector (ctx, bound, depth, ys)
        in
          (decs @ decs', PL.NewExp (x', ys'))
        end
    | goExp (ctx, bound, depth, f as PL.FunctionExp (params, body)) =
        if depth > 200 then
          let
            val fv = freeVarsExp (PL.IdSet.empty, f) PL.IdSet.empty
            val captures = PL.IdSet.toList (PL.IdSet.intersection (bound, fv))
            val bound' =
              Vector.foldl (fn (id, bound) => PL.IdSet.add (bound, id)) bound
                params
            val (decs, body') = goBlock (ctx, bound', 0, body)
            val name = freshVId (ctx, "f")
            val params' = Vector.foldr (op::) [] params
            val capturesAndParams = Vector.fromList (captures @ params')
            val newDec = PL.ConstStat (vector
              [(name, PL.FunctionExp (capturesAndParams, body'))])
            val newExp = PL.FunctionExp (params, vector
              [PL.ReturnStat (SOME (PL.CallExp
                 ( PL.VarExp (PL.UserDefinedId name)
                 , Vector.map PL.VarExp capturesAndParams
                 )))])
          in
            (decs @ [newDec], newExp)
          end
        else
          let
            val bound' =
              Vector.foldl (fn (id, bound) => PL.IdSet.add (bound, id)) bound
                params
            val (decs, body') = goBlock (ctx, bound', depth + 1, body)
          in
            (decs, PL.FunctionExp (params, body'))
          end
    | goExp (ctx, bound, depth, PL.BinExp (p, x, y)) =
        let
          val (decs, x') = goExp (ctx, bound, depth, x)
          val (decs', y') = goExp (ctx, bound, depth, y)
        in
          (decs @ decs', PL.BinExp (p, x', y'))
        end
    | goExp (ctx, bound, depth, PL.UnaryExp (p, x)) =
        let val (decs, x') = goExp (ctx, bound, depth, x)
        in (decs, PL.UnaryExp (p, x'))
        end
    | goExp (ctx, bound, depth, PL.IndexExp (x, y)) =
        let
          val (decs, x') = goExp (ctx, bound, depth, x)
          val (decs', y') = goExp (ctx, bound, depth, y)
        in
          (decs @ decs', PL.IndexExp (x', y'))
        end
    | goExp (ctx, bound, depth, PL.CondExp (x, y, z)) =
        let
          val (decs, x') = goExp (ctx, bound, depth, x)
          val (decs', y') = goExp (ctx, bound, depth, y)
          val (decs'', z') = goExp (ctx, bound, depth, z)
        in
          (decs @ decs' @ decs'', PL.CondExp (x', y', z'))
        end
  and goExpVector (ctx, bound, depth, xs) =
    let
      val (decs, ys) =
        Vector.foldr
          (fn (exp, (decs, ys)) =>
             let val (decs', exp') = goExp (ctx, bound, depth, exp)
             in (decs' @ decs, exp' :: ys)
             end) ([], []) xs
    in
      (decs, Vector.fromList ys)
    end
  and goStat (ctx, bound, depth, PL.LetStat vars) =
        let
          val (decs, vars) =
            Vector.foldr
              (fn ((vid, NONE), (decs, vars)) => (decs, (vid, NONE) :: vars)
                | ((vid, SOME exp), (decs, vars)) =>
                 let val (decs', exp) = goExp (ctx, bound, depth, exp)
                 in (decs' @ decs, (vid, SOME exp) :: vars)
                 end) ([], []) vars
        in
          (decs, PL.LetStat (Vector.fromList vars))
        end
    | goStat (ctx, bound, depth, PL.ConstStat vars) =
        let
          val (decs, vars) =
            Vector.foldr
              (fn ((vid, exp), (decs, vars)) =>
                 let val (decs', exp) = goExp (ctx, bound, depth, exp)
                 in (decs' @ decs, (vid, exp) :: vars)
                 end) ([], []) vars
        in
          (decs, PL.ConstStat (Vector.fromList vars))
        end
    | goStat (ctx, bound, depth, PL.ExpStat exp) =
        let val (decs, exp) = goExp (ctx, bound, depth, exp)
        in (decs, PL.ExpStat exp)
        end
    | goStat (ctx, bound, depth, PL.IfStat (exp, then', else')) =
        let
          val (decs, exp) = goExp (ctx, bound, depth, exp)
          val (decs', then') = goBlock (ctx, bound, depth, then')
          val (decs'', else') = goBlock (ctx, bound, depth, else')
        in
          (decs @ decs' @ decs'', PL.IfStat (exp, then', else'))
        end
    | goStat (_, _, _, s as PL.ReturnStat NONE) = ([], s)
    | goStat (ctx, bound, depth, PL.ReturnStat (SOME exp)) =
        let val (decs, exp) = goExp (ctx, bound, depth, exp)
        in (decs, PL.ReturnStat (SOME exp))
        end
    | goStat (ctx, bound, depth, PL.TryCatchStat (try, vid, catch)) =
        let
          val (decs, try) = goBlock (ctx, bound, depth, try)
          val (decs', catch) = goBlock
            (ctx, PL.IdSet.add (bound, PL.UserDefinedId vid), depth, catch)
        in
          (decs @ decs', PL.TryCatchStat (try, vid, catch))
        end
    | goStat (ctx, bound, depth, PL.ThrowStat exp) =
        let val (decs, exp) = goExp (ctx, bound, depth, exp)
        in (decs, PL.ThrowStat exp)
        end
    | goStat (ctx, bound, depth, PL.BlockStat (optLabel, block)) =
        let val (decs, block) = goBlock (ctx, bound, depth, block)
        in (decs, PL.BlockStat (optLabel, block))
        end
    | goStat (ctx, bound, depth, PL.LoopStat (optLabel, block)) =
        let val (decs, block) = goBlock (ctx, bound, depth, block)
        in (decs, PL.LoopStat (optLabel, block))
        end
    | goStat (ctx, bound, depth, PL.SwitchStat (exp, cases)) =
        let
          val (decs, exp) = goExp (ctx, bound, depth, exp)
          val (decs', cases) =
            List.foldr
              (fn ((c, block), (decs, cases)) =>
                 let val (decs', block) = goBlock (ctx, bound, depth, block)
                 in (decs' @ decs, (c, block) :: cases)
                 end) ([], []) cases
        in
          (decs @ decs', PL.SwitchStat (exp, cases))
        end
    | goStat (_, _, _, s as PL.BreakStat _) = ([], s)
    | goStat (_, _, _, s as PL.ContinueStat _) = ([], s)
    | goStat (ctx, bound, depth, PL.DefaultExportStat exp) =
        let val (decs, exp) = goExp (ctx, bound, depth, exp)
        in (decs, PL.DefaultExportStat exp)
        end
    | goStat (_, _, _, s as PL.NamedExportStat _) = ([], s)
  and goBlock (ctx, bound, depth, stats) =
    let
      val bound' = collectLetConstBlock stats bound
      val (decs, ys) =
        Vector.foldr
          (fn (stat, (decs, ys)) =>
             let val (decs', stat') = goStat (ctx, bound', depth, stat)
             in (decs' @ decs, stat' :: ys)
             end) ([], []) stats
    in
      (decs, Vector.fromList ys)
    end

  fun doProgram ctx block =
    let val (decs, block') = goBlock (ctx, PL.IdSet.empty, 0, block)
    in Vector.fromList (decs @ Vector.foldr (op::) [] block')
    end

end;
