signature BACKEND = sig
  type ctype
  type top_cast
  type cast

  val convert : AST.tast -> cast
  val convertTop : AST.top_ast -> top_cast
end
