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

  type tenv = ty symtab

  local
      open Parser
  in
    fun parseTypeSpecifier (Symbol "unit") _ = Unit
      | parseTypeSpecifier (Symbol "bool") _ = Bool
      | parseTypeSpecifier (Symbol "i64") _ = I64
      | parseTypeSpecifier (Symbol s) e = lookup s e
      | parseTypeSpecifier _ _ = raise Fail "Bad type specifier"
  end
end
