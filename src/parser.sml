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

structure Parser :> PARSER = struct
  datatype sexp = Integer of int
                | String of string
                | Symbol of string
                | SList of sexp list

  structure ps = Parsimony(ParsimonyStringInput)
  open ps

  val digitParser = anyOf [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9"]

  fun parseInt str = case (Int.fromString str) of
                         SOME i => i
                       | NONE => raise Match

  val naturalParser = pmap (parseInt o String.implode) (many1 digitParser)

  datatype sign = Positive | Negative

  val signParser = let val posParser = seqR (opt (pchar #"+")) (preturn Positive)
                       val negParser = seqR (pchar #"-") (preturn Negative)
                   in
                       or negParser posParser
                   end

  fun applySign (Positive, int) = int
    | applySign (Negative, int) = ~int

  val integerParser = pmap applySign (seq signParser naturalParser)

  val stringChar = or (seqR (pchar #"\\") (pchar #"\"")) (noneOf [#"\""])

  val quotedString = pmap String.implode (between (pchar #"\"") (many stringChar) (pchar #"\""))

  val symbolChar = anyOfString "abcdefghijklmnopqrstuvwxyz0123456789+-*/><='"

  val symbolParser = pmap String.implode (many1 symbolChar)

  val whitespaceParser = choice [pchar #" ",
                                 pchar #"\n",
                                 seqR (pchar #";")
                                      (seqR (many (noneOf [#"\n"]))
                                            (pchar #"\n"))]

  val ws = many whitespaceParser

  fun defineSexpParser listParser =
    seqR ws (choice [pmap Integer integerParser,
                     pmap String quotedString,
                     pmap Symbol symbolParser,
                     listParser])

  val listParser =
      let val (sexpParser: sexp parser, r: sexp parser ref) = wrapper ()
      in
          let val listParser = pmap SList (seqR (pchar #"(")
                                                (between ws
                                                         (many (seqL sexpParser ws))
                                                         (pchar #")")))
          in
              r := defineSexpParser listParser;
              listParser
          end
      end

  val sexpParser = defineSexpParser listParser

  fun parseString s =
    case (run sexpParser (ParsimonyStringInput.fromString s)) of
        (Success r) => r
      | f => raise Fail ("Bad parse: " ^ (explain f))
end
