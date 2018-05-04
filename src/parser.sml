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

  val symbolChar = anyOfString "abcdefghijklmnoprstuvwxyz-0123456789"

  val symbolParser = pmap String.implode (many1 symbolChar)

  val whitespaceParser = anyOf [#" ", #"\n"]

  val ws = many whitespaceParser

  fun defineSexpParser listParser =
    ps.choice [ps.pmap Integer integerParser,
               ps.pmap String quotedString,
               ps.pmap Symbol symbolParser,
               listParser]

  val listParser =
      let val (sexpParser: sexp ps.parser, r: sexp ps.parser ref) = ps.wrapper ()
      in
          let val listParser = ps.pmap SList (ps.between (ps.pchar #"(")
                                                         (ps.many (ps.seqL sexpParser ws))
                                                         (ps.pchar #")"))
          in
              r := defineSexpParser listParser;
              listParser
          end
      end

  val sexpParser = defineSexpParser listParser

  fun parseString s =
    case (run sexpParser (ParsimonyStringInput.fromString s)) of
        (Success (v, p)) => v
      | (Failure msg) => raise Fail "Bad parse"
end
