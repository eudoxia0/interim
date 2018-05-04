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

  datatype ast = ConstBool of bool
               | ConstInt of int
               | ConstString of string
               | Var of string
               | Binop of binop * ast * ast
               | Cond of ast * ast * ast
               | Funcall of string * ast list

  datatype top_ast = Defun of Function.func * ast

  val parse : Parser.sexp -> ast
  val parseToplevel : Parser.sexp -> Type.tenv -> top_ast

  datatype tast = TConstBool of bool
                | TConstInt of int * Type.ty
                | TVar of string * Type.ty
                | TBinop of binop * tast * tast * Type.ty
                | TCond of tast * tast * tast * Type.ty
                | TFuncall of string * tast list * Type.ty

  val typeOf : tast -> Type.ty

  val augment : ast -> Function.stack -> Type.tenv -> Function.fenv -> tast
end
