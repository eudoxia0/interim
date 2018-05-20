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

signature TAST = sig
  datatype tast = TConstUnit
                | TConstBool of bool
                | TConstInt of int * Type.ty
                | TConstString of string
                | TVar of string * Type.ty
                | TBinop of AST.binop * tast * tast * Type.ty
                | TCond of tast * tast * tast * Type.ty
                | TCast of Type.ty * tast
                | TProgn of tast list
                | TLet of string * tast * tast
                | TAssign of string * tast
                | TNullPtr of Type.ty
                | TLoad of tast * Type.ty
                | TStore of tast * tast
                | TMalloc of Type.ty * tast
                | TFree of tast
                | TAddressOf of string * Type.ty
                | TPrint of tast * AST.newline
                | TCEmbed of Type.ty * string
                | TCCall of string * Type.ty * tast list
                | TWhile of tast * tast
                | TLetRegion of Type.region * tast
                | TAllocate of Type.region * tast
                | TNullableCase of tast * string * tast * tast * Type.ty
                | TMakeRecord of Type.ty * string * (string * tast) list
                | TSlotAccess of tast * string * Type.ty
                | TFuncall of string * tast list * Type.ty

  val typeOf : tast -> Type.ty

  type context
  val mkContext : Function.stack -> Type.tenv -> Function.fenv -> Type.renv -> context

  val augment : AST.ast -> context -> tast
end
