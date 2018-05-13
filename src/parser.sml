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

  val symbolChar = anyOfString "abcdefghijklmnopqrstuvwxyz0123456789+-*/><="

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
        (Success (v, p)) => v
      | (Failure (_, _, msg)) => raise Fail ("Bad parse: " ^ msg)
end
