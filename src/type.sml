structure Type :> TYPE = struct
  open SymTab

  datatype ty = Unit
              | Bool
              | I64

  type tenv = ty symtab

  local
      open Parser
  in
    fun parseTypeSpecifier (Symbol "unit") _ = SOME Unit
      | parseTypeSpecifier (Symbol "bool") _ = SOME Bool
      | parseTypeSpecifier (Symbol "i64") _ = SOME I64
      | parseTypeSpecifier (Symbol s) e = lookup s e
      | parseTypeSpecifier _ _ = NONE
  end
end
