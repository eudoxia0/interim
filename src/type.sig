signature TYPE = sig
  datatype ty = Unit
              | Bool
              | U8
              | I8
              | U16
              | I16
              | U32
              | I32
              | U64
              | I64
              | RawPointer of ty
              | Record of string * slot list
       and slot = Slot of string * ty

  val isEquatable : ty -> bool
  val isNumeric : ty -> bool
  val isPrintable : ty -> bool

  type tenv = ty SymTab.symtab

  val parseTypeSpecifier : Parser.sexp -> tenv -> ty
end
