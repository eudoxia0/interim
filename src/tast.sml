structure TAST :> TAST = struct
  open SymTab

  datatype tast = TConstUnit
                | TConstBool of bool
                | TConstInt of int * Type.ty
                | TVar of string * Type.ty
                | TBinop of AST.binop * tast * tast * Type.ty
                | TCond of tast * tast * tast * Type.ty
                | TCast of Type.ty * tast
                | TProgn of tast list
                | TLet of string * tast * tast
                | TNullPtr of Type.ty
                | TLoad of tast * Type.ty
                | TStore of tast * tast
                | TMalloc of Type.ty * tast
                | TFree of tast
                | TPrint of tast
                | TCEmbed of Type.ty * string
                | TFuncall of string * tast list * Type.ty

  local
      open Type
      open Function
  in
    fun typeOf TConstUnit = Unit
      | typeOf (TConstBool _) = Bool
      | typeOf (TConstInt (_, t)) = t
      | typeOf (TVar (_, t)) = t
      | typeOf (TBinop (_, _, _, t)) = t
      | typeOf (TCond (_, _, _, t)) = t
      | typeOf (TCast (t, _)) = t
      | typeOf (TProgn ls) =
        if (length ls = 0) then
            Unit
        else
            typeOf (List.last ls)
      | typeOf (TLet (_, _, b)) = typeOf b
      | typeOf (TNullPtr t) = RawPointer t
      | typeOf (TLoad (_, t)) = t
      | typeOf (TStore (_, v)) = typeOf v
      | typeOf (TMalloc (t, _)) = RawPointer t
      | typeOf (TFree _) = Unit
      | typeOf (TPrint _) = Unit
      | typeOf (TCEmbed (t, _)) = t
      | typeOf (TFuncall (_, _, t)) = t

    fun matchTypes (params: param list) (args: tast list) =
      if (length params <> length args) then
          raise Fail "Wrong parameter count"
      else
          ListPair.all (fn (pt, at) => pt = at)
                       ((map (fn (Function.Param (n,t)) => t) params),
                        (map typeOf args))

    local
        open AST
    in
      fun augment ConstUnit _ _ _ = TConstUnit
        | augment (ConstBool b) _ _ _ = TConstBool b
        | augment (ConstInt i) _ _ _ = TConstInt (i, I64)
        | augment (ConstString s) _ _ _ = raise Fail "STRINGS ARE NOT SUPPORTED YET"
        | augment (Var s) stack _ _ = TVar (s, bindType (lookup s stack))
        | augment (Binop (Add, a, b)) s t f = augmentArithOp Add a b s t f
        | augment (Binop (Sub, a, b)) s t f = augmentArithOp Sub a b s t f
        | augment (Binop (Mul, a, b)) s t f = augmentArithOp Mul a b s t f
        | augment (Binop (Div, a, b)) s t f = augmentArithOp Div a b s t f
        | augment (Binop (Eq, a, b)) s t f = augmentCompOp Eq a b s t f
        | augment (Binop (LT, a, b)) s t f = augmentCompOp LT a b s t f
        | augment (Binop (LEq, a, b)) s t f = augmentCompOp LEq a b s t f
        | augment (Binop (GT, a, b)) s t f = augmentCompOp GT a b s t f
        | augment (Binop (GEq, a, b)) s t f = augmentCompOp GEq a b s t f
        | augment (Cast (ty, a)) s t f =
          let val a' = augment a s t f
          in
              if (Type.isNumeric ty) then
                  if (Type.isNumeric (typeOf a')) then
                      TCast (ty, a')
                  else
                      raise Fail "Cannot cast this type to a numeric type"
              else
                  raise Fail "Casting to this type is not supported"
          end
        | augment (Cond (test, c, a)) s t f =
          let val test' = augment test s t f
              and c' = augment c s t f
              and a' = augment a s t f
          in
              if (typeOf test') <> Bool then
                  raise Fail "The test in an if must be of boolean type"
              else
                  if (typeOf c') <> (typeOf a') then
                      raise Fail "The consequent and the alternate must have the same type"
                  else
                      TCond (test', c', a', typeOf c')
          end
        | augment (Progn exps) s t f =
          TProgn (map (fn a => augment a s t f) exps)
        | augment (Let (name, v, body)) s t f =
          let val v' = augment v s t f
          in
              let val s' = bind (name, (Binding (name, typeOf v'))) s
              in
                  TLet (name,
                        v',
                        augment body s' t f)
              end
          end
        | augment (NullPtr t) _ tenv _ =
          TNullPtr (parseTypeSpecifier t tenv)
        | augment (Load e) s t f =
          let val e' = augment e s t f
          in
              case (typeOf e') of
                  RawPointer t => TLoad (e', t)
                | _ => raise Fail "load: not a pointer"
          end
        | augment (Store (p, v)) s t f =
          let val p' = augment p s t f
              and v' = augment v s t f
          in
              case (typeOf p') of
                  RawPointer t => let val ty = typeOf v'
                                  in
                                      if ty = t then
                                          TStore (p', v')
                                      else
                                          raise Fail "store: type mismatch"
                                  end
                | _ => raise Fail "store: first argument must be a pointer"
          end
        | augment (Malloc (ty, c)) s t f =
          let val t' = parseTypeSpecifier ty t
              and c' = augment c s t f
          in
              if (typeOf c' <> U64) then
                  raise Fail "malloc: allocation count must be u64"
              else
                  TMalloc (t', c')
          end
        | augment (Free p) s t f =
          let val p' = augment p s t f
          in
              case (typeOf p') of
                  (RawPointer _) => TFree p'
                | _ => raise Fail "Can't free a non-pointer"
          end
        | augment (Print v) s t f =
          let val v' = augment v s t f
          in
              if isPrintable (typeOf v') then
                  TPrint (v')
              else
                  raise Fail "Type cannot be printed"
          end
        | augment (CEmbed (ts, c)) _ t _ =
          TCEmbed (parseTypeSpecifier ts t, c)
        | augment (Funcall (name, args)) s t fenv =
          let val (Function (_, params, rt)) = lookup name fenv
              and targs = (map (fn e => augment e s t fenv) args)
          in
              if matchTypes params targs then
                  TFuncall (name, targs, rt)
              else
                  raise Fail "Argument types don't match parameter types"
          end
      and augmentArithOp oper a b s t f =
          let val a' = augment a s t f
              and b' = augment b s t f
          in
              if (typeOf a') <> (typeOf b') then
                  raise Fail "Both operands to an arithmetic operation must be of the same type"
              else
                  let val t = typeOf a'
                  in
                      if (isNumeric t) then
                          TBinop (oper, a', b', t)
                      else
                          raise Fail "Can't perform arithmetic on non-numeric types"
                  end
          end
      and augmentCompOp oper a b s t f =
          let val a' = augment a s t f
              and b' = augment b s t f
          in
              if (typeOf a') <> (typeOf b') then
                  raise Fail "Both operands to an comparison operation must be of the same type"
              else
                  if isEquatable (typeOf a') then
                      TBinop (oper, a', b', Bool)
                  else
                      raise Fail "Cannot compare objects of this type"
          end
    end
  end
end
