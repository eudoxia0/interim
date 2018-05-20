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

signature FUNCTION = sig
  datatype param = Param of string * Type.pty
  datatype conc_param = ConcParam of string * Type.ty
  datatype func = Function of string * param list * Type.ty

  type fenv = func SymTab.symtab

  datatype mutability = Mutable
                      | Immutable

  datatype binding = Binding of string * Type.ty * mutability
  type stack = binding SymTab.symtab

  val bindType : binding -> Type.ty

  val funcName : func -> string
  val funcRT : func -> Type.ty

  val matchParams : param list -> Type.ty list -> conc_param list

  val toStack : func -> stack
end
