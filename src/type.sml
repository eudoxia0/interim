structure Type :> TYPE = struct
  open SymTab

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

  fun isNumeric U8 = true
    | isNumeric I8 = true
    | isNumeric U16 = true
    | isNumeric I16 = true
    | isNumeric U32 = true
    | isNumeric I32 = true
    | isNumeric U64 = true
    | isNumeric I64 = true
    | isNumeric _ = false

  type tenv = ty symtab

  local
      open Parser
  in
    fun parseTypeSpecifier (Symbol "unit") _ = Unit
      | parseTypeSpecifier (Symbol "bool") _ = Bool
      | parseTypeSpecifier (Symbol "u8") _ = U8
      | parseTypeSpecifier (Symbol "i8") _ = I8
      | parseTypeSpecifier (Symbol "u16") _ = U16
      | parseTypeSpecifier (Symbol "i16") _ = I16
      | parseTypeSpecifier (Symbol "u32") _ = U32
      | parseTypeSpecifier (Symbol "i32") _ = I32
      | parseTypeSpecifier (Symbol "u64") _ = U64
      | parseTypeSpecifier (Symbol "i64") _ = I64
      | parseTypeSpecifier (SList [Symbol "rawptr", t]) e = RawPointer (parseTypeSpecifier t e)
      | parseTypeSpecifier (Symbol s) e = lookup s e
      | parseTypeSpecifier _ _ = raise Fail "Bad type specifier"
  end
end
