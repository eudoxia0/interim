signature TYPE = sig
  datatype ty = Unit
              | Bool
              | I64

  type tenv = ty SymTab.symtab

  val parseTypeSpecifier : Parser.sexp -> tenv -> ty option
end
