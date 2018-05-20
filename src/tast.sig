signature TAST = sig
  datatype tast = TConstUnit
                | TConstBool of bool
                | TConstInt of int * Type.ty
                | TConstString of string
                | TVar of string * Type.ty
                | TBinop of AST.binop * tast * tast * Type.ty
                | TCond of tast * tast * tast * Type.ty
                | TCast of Type.ty * tast
                | TProgn of tast list
                | TLet of string * tast * tast
                | TAssign of string * tast
                | TNullPtr of Type.ty
                | TLoad of tast * Type.ty
                | TStore of tast * tast
                | TMalloc of Type.ty * tast
                | TFree of tast
                | TAddressOf of string * Type.ty
                | TPrint of tast * AST.newline
                | TCEmbed of Type.ty * string
                | TCCall of string * Type.ty * tast list
                | TWhile of tast * tast
                | TLetRegion of Type.region * tast
                | TAllocate of Type.region * tast
                | TMakeRecord of Type.ty * string * (string * tast) list
                | TSlotAccess of tast * string * Type.ty
                | TFuncall of string * tast list * Type.ty

  val typeOf : tast -> Type.ty

  type context
  val mkContext : Function.stack -> Type.tenv -> Function.fenv -> Type.renv -> context

  val augment : AST.ast -> context -> tast
end
