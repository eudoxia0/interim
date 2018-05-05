structure AST :> AST = struct
  open SymTab

  datatype binop = Add
                 | Sub
                 | Mul
                 | Div
                 | Eq
                 | LT
                 | LEq
                 | GT
                 | GEq

  datatype ast = ConstUnit
               | ConstBool of bool
               | ConstInt of int
               | ConstString of string
               | Var of string
               | Binop of binop * ast * ast
               | Cond of ast * ast * ast
               | Funcall of string * ast list

  datatype top_ast = Defun of Function.func * ast

  local
    open Parser
  in
    fun parse (Integer i) = ConstInt i
      | parse (String s) = ConstString s
      | parse (Symbol "nil") = ConstUnit
      | parse (Symbol "true") = ConstBool true
      | parse (Symbol "false") = ConstBool false
      | parse (Symbol s) = Var s
      | parse (SList [Symbol "+", a, b]) = Binop (Add, parse a, parse b)
      | parse (SList [Symbol "-", a, b]) = Binop (Sub, parse a, parse b)
      | parse (SList [Symbol "*", a, b]) = Binop (Mul, parse a, parse b)
      | parse (SList [Symbol "/", a, b]) = Binop (Div, parse a, parse b)
      | parse (SList [Symbol "=", a, b]) = Binop (Eq, parse a, parse b)
      | parse (SList [Symbol "<", a, b]) = Binop (LT, parse a, parse b)
      | parse (SList [Symbol "<=", a, b]) = Binop (LEq, parse a, parse b)
      | parse (SList [Symbol ">", a, b]) = Binop (GT, parse a, parse b)
      | parse (SList [Symbol ">=", a, b]) = Binop (GEq, parse a, parse b)
      | parse (SList [Symbol "if", t, c, a]) = Cond (parse t, parse c, parse a)
      | parse (SList ((Symbol s)::rest)) = Funcall (s, map parse rest)
      | parse _ = raise Fail "Bad expression"

    fun parseParam (SList [Symbol n, t]) e = Function.Param (n, Type.parseTypeSpecifier t e)
      | parseParam _ _ = raise Fail "Bad parameter"

    fun parseToplevel (SList [Symbol "defun", Symbol name, SList params, rt, body]) e =
      Defun (Function.Function (name,
                                map (fn p => parseParam p e) params,
                                Type.parseTypeSpecifier rt e),
             parse body)
      | parseToplevel _ _ = raise Fail "Bad toplevel node"
  end

  datatype tast = TConstUnit
                | TConstBool of bool
                | TConstInt of int * Type.ty
                | TVar of string * Type.ty
                | TBinop of binop * tast * tast * Type.ty
                | TCond of tast * tast * tast * Type.ty
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
      | typeOf (TFuncall (_, _, t)) = t

    fun matchTypes (params: param list) (args: tast list) =
      if (length params <> length args) then
          raise Fail "Wrong parameter count"
      else
          ListPair.all (fn (pt, at) => pt = at)
                       ((map (fn (Function.Param (n,t)) => t) params),
                        (map typeOf args))

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
                TBinop (oper, a', b', typeOf a')
        end
    and augmentCompOp oper a b s t f =
        let val a' = augment a s t f
            and b' = augment b s t f
        in
            if (typeOf a') <> (typeOf b') then
                raise Fail "Both operands to an comparison operation must be of the same type"
            else
                TBinop (oper, a', b', Bool)
        end
  end
end
