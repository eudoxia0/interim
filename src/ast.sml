structure AST :> AST = struct
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

  type fun_name = string
  datatype param = Param of string * Type.ty

  datatype top_ast = Defun of fun_name * param list * Type.ty * ast

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

    fun parseParam (SList [Symbol n, t]) e = Param (n, Type.parseTypeSpecifier t e)
      | parseParam _ _ = raise Fail "Bad parameter"

    fun parseToplevel (SList [Symbol "defun", Symbol name, SList params, rt, body]) e =
      Defun (name,
             map (fn p => parseParam p e) params,
             Type.parseTypeSpecifier rt e,
             parse body)
      | parseToplevel _ _ = raise Fail "Bad toplevel node"
  end
end
