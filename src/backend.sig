signature BACKEND = sig
  type ctype
  type exp_cast
  type block_cast
  type top_cast

  val convertType : Type.ty -> ctype
  val convert : TAST.tast -> block_cast * exp_cast
  val defineFunction : Function.func -> TAST.tast -> top_cast
  val defineType : string -> Type.ty -> top_cast
  val renderExp : exp_cast -> string
  val renderBlock : block_cast -> string
  val renderTop : top_cast -> string
end
