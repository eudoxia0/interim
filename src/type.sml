(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Interim.

    Interim is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Interim is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Interim.  If not, see <http://www.gnu.org/licenses/>.
*)

structure Type :> TYPE = struct
  open SymTab

  datatype ty = Unit
              | Bool
              | Int of signedness * bit_width
              | Str
              | RawPointer of ty
              | Record of string * slot list
              | RegionType of region
              | RegionPointer of ty * region
              | NullablePointer of ty * region
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
    | tyToString (RegionPointer (ty, (Region (_, name)))) = "(pointer " ^ (tyToString ty) ^ " " ^ name ^ ")"
    | tyToString (NullablePointer (ty, (Region (_, name)))) = "(nullable " ^ (tyToString ty) ^ " " ^ name ^ ")"
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
               | PRegionPointer of pty * string
               | PNullablePointer of pty * string

  fun toParamType Unit = PUnit
    | toParamType Bool = PBool
    | toParamType (Int i) = PInt i
    | toParamType Str = PStr
    | toParamType (RawPointer t) = PRawPointer (toParamType t)
    | toParamType (Record d) = PRecord d
    | toParamType (RegionType _) = raise Fail "Can't do this"
    | toParamType (RegionPointer _)  = raise Fail "Can't do this"
    | toParamType (NullablePointer _)  = raise Fail "Can't do this"

  type tenv = ty symtab
  type renv = region SymTab.symtab

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
      | parseParamTypeSpecifier (SList [Symbol "pointer", ty, Symbol p]) e =
        PRegionPointer (parseParamTypeSpecifier ty e, p)
      | parseParamTypeSpecifier (SList [Symbol "nullable", ty, Symbol p]) e =
        PNullablePointer (parseParamTypeSpecifier ty e, p)
      | parseParamTypeSpecifier f e = toParamType (parseTypeSpecifier f e)
  end
end
