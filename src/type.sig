signature TYPE = sig
  datatype ty = Unit
              | Bool
              | Int of signedness * bit_width
              | Str
              | RawPointer of ty
              | Record of string * slot list
              | RegionType of region
       and signedness = Signed | Unsigned
       and bit_width = Word8 | Word16 | Word32 | Word64
       and slot = Slot of string * ty
       and region = Region of int * string

  val isEquatable : ty -> bool
  val isNumeric : ty -> bool
  val isPrintable : ty -> bool
  val tyToString : ty -> string

  datatype pty = PUnit
               | PBool
               | PInt of signedness * bit_width
               | PStr
               | PRawPointer of pty
               | PRecord of string * slot list
               | RegionParam of string

  val toParamType : ty -> pty

  type tenv = ty SymTab.symtab

  val parseTypeSpecifier : Parser.sexp -> tenv -> ty
  val parseParamTypeSpecifier : Parser.sexp -> tenv -> pty
end
