signature AST = sig
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

  val parseSexp : Parser.sexp -> ast option
end
