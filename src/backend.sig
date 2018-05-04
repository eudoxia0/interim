signature BACKEND = sig
  type ctype
  type top_cast
  type cast

  val convertType : Type.ty -> ctype
  val convert : AST.tast -> cast
  val defineFunction : Function.func -> AST.tast -> top_cast
end
