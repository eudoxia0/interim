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

  datatype ast = ConstInt of int
               | ConstString of string
               | Var of string
               | Binop of binop * ast * ast
               | Funcall of string * ast list

  datatype top_ast = Defun of Function.func * ast

  local
    open Parser
  in
    fun parse (Integer i) = ConstInt i
      | parse (String s) = ConstString s
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

  datatype tast = TConstInt of int * Type.ty
                | TVar of string * Type.ty
                | TBinop of binop * tast * tast * Type.ty
                | TFuncall of string * tast list * Type.ty

  local
      open Type
      open Function
  in
    fun typeOf (TConstInt (_, t)) = t
      | typeOf (TVar (_, t)) = t
      | typeOf (TBinop (_, _, _, t)) = t
      | typeOf (TFuncall (_, _, t)) = t

    fun matchTypes (params: param list) (args: tast list) =
      if (length params <> length args) then
          raise Fail "Wrong parameter count"
      else
          ListPair.all (fn (pt, at) => pt = at)
                       ((map (fn (Function.Param (n,t)) => t) params),
                        (map typeOf args))

    fun augment (ConstInt i) _ _ _ = TConstInt (i, I64)
      | augment (ConstString s) _ _ _ = raise Fail "STRINGS ARE NOT SUPPORTED YET"
      | augment (Var s) stack _ _ = TVar (s, bindType (lookup s stack))
      | augment (Binop (Add, a, b)) s t f = TBinop (Add, augment a s t f, augment b s t f, I64)
      | augment (Binop (Sub, a, b)) s t f = TBinop (Sub, augment a s t f, augment b s t f, I64)
      | augment (Binop (Mul, a, b)) s t f = TBinop (Mul, augment a s t f, augment b s t f, I64)
      | augment (Binop (Div, a, b)) s t f = TBinop (Div, augment a s t f, augment b s t f, I64)
      | augment (Binop (LT, a, b)) s t f = TBinop (LT, augment a s t f, augment b s t f, Bool)
      | augment (Binop (LEq, a, b)) s t f = TBinop (LEq, augment a s t f, augment b s t f, Bool)
      | augment (Binop (GT, a, b)) s t f = TBinop (GT, augment a s t f, augment b s t f, Bool)
      | augment (Binop (GEq, a, b)) s t f = TBinop (GEq, augment a s t f, augment b s t f, Bool)
      | augment (Funcall (name, args)) s t fenv =
        let val (Function (_, params, rt)) = lookup name fenv
            and targs = (map (fn e => augment e s t fenv) args)
        in
            if matchTypes params targs then
                TFuncall (name, targs, rt)
            else
                raise Fail "Argument types don't match parameter types"
        end
      | augment _ _ _ _ = raise Fail "DERP"
  end
end
