signature BACKEND = sig
  type ctype
  type exp_cast
  type block_cast
  type top_cast

  val convertType : Type.ty -> ctype
  val convert : AST.tast -> block_cast * exp_cast
  val defineFunction : Function.func -> AST.tast -> top_cast
  val renderExp : exp_cast -> string
  val renderBlock : block_cast -> string
  val renderTop : top_cast -> string
end
