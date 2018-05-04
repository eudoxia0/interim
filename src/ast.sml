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
  type param = string * Type.ty

  datatype top_ast = Defun of fun_name * param list * ast

  local
    open Parser
  in
    fun p (Integer i) = ConstInt i
      | p (String s) = ConstString s
      | p (Symbol s) = Var s
      | p (SList [Symbol "+", a, b]) = Binop (Add, p a, p b)
      | p (SList [Symbol "-", a, b]) = Binop (Sub, p a, p b)
      | p (SList [Symbol "*", a, b]) = Binop (Mul, p a, p b)
      | p (SList [Symbol "/", a, b]) = Binop (Div, p a, p b)
      | p (SList [Symbol "=", a, b]) = Binop (Eq, p a, p b)
      | p (SList [Symbol "<", a, b]) = Binop (LT, p a, p b)
      | p (SList [Symbol "<=", a, b]) = Binop (LEq, p a, p b)
      | p (SList [Symbol ">", a, b]) = Binop (GT, p a, p b)
      | p (SList [Symbol ">=", a, b]) = Binop (GEq, p a, p b)
      | p _ = raise Fail "Bad sexp"

    fun parseSexp s = (SOME (p s)) handle (Fail _) => NONE
  end
end
