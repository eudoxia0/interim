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

  datatype param = Param of string * Type.ty
  datatype func = Function of string * param list * Type.ty

  val funcName : func -> string

  datatype top_ast = Defun of func * ast

  val parse : Parser.sexp -> ast
  val parseToplevel : Parser.sexp -> Type.tenv -> top_ast

  type fenv = func SymTab.symtab
  datatype tast = TConstInt of int * Type.ty
                | TVar of string * Type.ty
                | TBinop of binop * tast * tast * Type.ty
                | TFuncall of string * tast list * Type.ty

  val typeOf : tast -> Type.ty

  datatype binding = Binding of string * Type.ty
  type stack = binding SymTab.symtab

  val augment : ast -> stack -> Type.tenv -> fenv -> tast
end
