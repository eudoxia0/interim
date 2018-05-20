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

local
  open Util
  open Compiler
in
  fun compile input output =
    let val c = compileFile emptyCompiler input
    in
        writeStringToFile output (compilerCode c)
    end
end

fun main () =
  case CommandLine.arguments() of
      [input, output] => compile input output
    | _ => raise Fail "Bad invocation"

val _ = main ()
