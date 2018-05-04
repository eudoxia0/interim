signature FUNCTION = sig
  datatype param = Param of string * Type.ty
  datatype func = Function of string * param list * Type.ty

  type fenv = func SymTab.symtab

  type binding
  type stack = binding SymTab.symtab

  val bindType : binding -> Type.ty

  val funcName : func -> string
  val funcStack : func -> stack
end
