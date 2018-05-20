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

signature BACKEND = sig
  type ctype
  type exp_cast
  type block_cast
  type top_cast

  val convertType : Type.ty -> ctype
  val convert : TAST.tast -> block_cast * exp_cast
  val defineFunction : Function.func -> TAST.tast -> top_cast
  val defineStruct : string -> Type.slot list -> top_cast
  val renderExp : exp_cast -> string
  val renderBlock : block_cast -> string
  val renderTop : top_cast -> string
end
