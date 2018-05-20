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

signature AST = sig
  datatype binop = Add
                 | Sub
                 | Mul
                 | Div
                 | Eq
                 | NEq
                 | LT
                 | LEq
                 | GT
                 | GEq

  val binopName : binop -> string

  datatype ast = ConstUnit
               | ConstBool of bool
               | ConstInt of int
               | ConstString of string
               | Var of string
               | Binop of binop * ast * ast
               | Cond of ast * ast * ast
               | Cast of Type.ty * ast
               | Progn of ast list
               | Let of string * ast * ast
               | Assign of string * ast
               | NullPtr of Parser.sexp
               | Load of ast
               | Store of ast * ast
               | Malloc of Parser.sexp * ast
               | Free of ast
               | AddressOf of string
               | Print of ast * newline
               | CEmbed of Parser.sexp * string
               | CCall of string * Parser.sexp * ast list
               | While of ast * ast
               | LetRegion of Type.region * ast
               | Allocate of string * ast
               | NullableCase of ast * string * ast * ast
               | MakeRecord of string * (string * ast) list
               | SlotAccess of ast * string
               | Funcall of string * ast list
       and newline = Newline
                   | NoNewline

  datatype top_ast = Defun of Function.func * ast
                   | Defrecord of string * (string * Type.ty) list
                   | CInclude of string

  val parse : Parser.sexp -> Type.tenv -> ast
  val parseToplevel : Parser.sexp -> Type.tenv -> top_ast
end
