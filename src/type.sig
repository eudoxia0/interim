signature TYPE = sig
  datatype ty = Unit
              | Bool
              | Int of signedness * bit_width
              | RawPointer of ty
              | Record of string * slot list
       and signedness = Signed | Unsigned
       and bit_width = Word8 | Word16 | Word32 | Word64
       and slot = Slot of string * ty

  val isEquatable : ty -> bool
  val isNumeric : ty -> bool
  val isPrintable : ty -> bool

  type tenv = ty SymTab.symtab

  val parseTypeSpecifier : Parser.sexp -> tenv -> ty
end
