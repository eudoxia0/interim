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
              | Tuple of ty list

  val isNumeric : ty -> bool

  type tenv = ty SymTab.symtab

  val parseTypeSpecifier : Parser.sexp -> tenv -> ty
end
