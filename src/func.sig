signature FUNCTION = sig
  datatype param = Param of string * Type.pty
  datatype conc_param = ConcParam of string * Type.ty
  datatype func = Function of string * param list * Type.ty

  type fenv = func SymTab.symtab

  datatype mutability = Mutable
                      | Immutable

  datatype binding = Binding of string * Type.ty * mutability
  type stack = binding SymTab.symtab

  val bindType : binding -> Type.ty

  val funcName : func -> string
  val funcRT : func -> Type.ty

  val matchParams : param list -> Type.ty list -> conc_param list

  val toStack : func -> stack
end
