structure Type :> TYPE = struct
  open SymTab

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

  fun isEquatable (Record _) = false
    | isEquatable _ = true

  fun isNumeric (Int _) = true
    | isNumeric _ = false

  fun isPrintable (Record _) = false
    | isPrintable _ = true

  fun tyToString Unit = "unit"
    | tyToString Bool = "bool"
    | tyToString (Int (s, w)) = (signednessStr s) ^ (widthStr w)
    | tyToString Str = "str"
    | tyToString (RawPointer t) = "(rawptr " ^ (tyToString t) ^ ")"
    | tyToString (Record (name, _)) = name
    | tyToString (RegionType (Region (_, name))) = "(region " ^ name ^ ")"
  and signednessStr Signed = "i"
    | signednessStr Unsigned = "u"
  and widthStr Word8 = "8"
    | widthStr Word16 = "16"
    | widthStr Word32 = "32"
    | widthStr Word64 = "64"

  datatype pty = PUnit
               | PBool
               | PInt of signedness * bit_width
               | PStr
               | PRawPointer of pty
               | PRecord of string * slot list
               | RegionParam of string

  fun toParamType Unit = PUnit
    | toParamType Bool = PBool
    | toParamType (Int i) = PInt i
    | toParamType Str = PStr
    | toParamType (RawPointer t) = PRawPointer (toParamType t)
    | toParamType (Record d) = PRecord d
    | toParamType (RegionType _) = raise Fail "Can't do this"

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
