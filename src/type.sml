structure Type :> TYPE = struct
  open SymTab

  datatype ty = Unit
              | Bool
              | Int of signedness * bit_width
              | Str
              | RawPointer of ty
              | Record of string * slot list
       and signedness = Signed | Unsigned
       and bit_width = Word8 | Word16 | Word32 | Word64
       and slot = Slot of string * ty

  fun isEquatable (Record _) = false
    | isEquatable _ = true

  fun isNumeric (Int _) = true
    | isNumeric _ = false

  fun isPrintable (Record _) = false
    | isPrintable _ = true

  datatype pty = PUnit
               | PBool
               | PInt of signedness * bit_width
               | PStr
               | PRawPointer of pty
               | RegionParam of string

  fun toParamType Unit = PUnit
    | toParamType Bool = PBool
    | toParamType (Int i) = PInt i
    | toParamType Str = PStr
    | toParamType (RawPointer t) = PRawPointer (toParamType t)
    | toParamType (Record _) = raise Fail "Records not supported yet"

  type tenv = ty symtab

  local
      open Parser
  in
    fun parseTypeSpecifier (Symbol "unit") _ = Unit
      | parseTypeSpecifier (Symbol "bool") _ = Bool
      | parseTypeSpecifier (Symbol "u8") _ = Int (Unsigned, Word8)
      | parseTypeSpecifier (Symbol "i8") _ = Int (Signed, Word8)
      | parseTypeSpecifier (Symbol "u16") _ = Int (Unsigned, Word16)
      | parseTypeSpecifier (Symbol "i16") _ = Int (Signed, Word16)
      | parseTypeSpecifier (Symbol "u32") _ = Int (Unsigned, Word32)
      | parseTypeSpecifier (Symbol "i32") _ = Int (Signed, Word32)
      | parseTypeSpecifier (Symbol "u64") _ = Int (Unsigned, Word64)
      | parseTypeSpecifier (Symbol "i64") _ = Int (Signed, Word64)
      | parseTypeSpecifier (Symbol "str") _ = Str
      | parseTypeSpecifier (SList [Symbol "rawptr", t]) e = RawPointer (parseTypeSpecifier t e)
      | parseTypeSpecifier (Symbol s) e = lookup s e
      | parseTypeSpecifier _ _ = raise Fail "Bad type specifier"

    fun parseParamTypeSpecifier (SList [Symbol "region", Symbol p]) _ = RegionParam p
      | parseParamTypeSpecifier f e = toParamType (parseTypeSpecifier f e)
  end
end
