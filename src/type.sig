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

signature TYPE = sig
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
               | PRegionPointer of pty * string
               | PNullablePointer of pty * string

  val toParamType : ty -> pty

  type tenv = ty SymTab.symtab
  type renv = region SymTab.symtab

  val parseTypeSpecifier : Parser.sexp -> tenv -> ty
  val parseParamTypeSpecifier : Parser.sexp -> tenv -> pty
end
