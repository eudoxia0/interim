signature BACKEND = sig
  type ctype
  type top_ast

  val convert : AST.top_ast -> top_ast
end
