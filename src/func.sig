signature FUNCTION = sig
  datatype param = Param of string * Type.ty
  datatype func = Function of string * param list * Type.ty

  type fenv = func SymTab.symtab

  datatype binding = Binding of string * Type.ty
  type stack = binding SymTab.symtab

  val bindType : binding -> Type.ty

  val funcName : func -> string
  val funcRT : func -> Type.ty
  val funcStack : func -> stack
end
