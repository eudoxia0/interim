signature PARSER = sig
  datatype sexp = Integer of int
                | String of string
                | Symbol of string
                | SList of sexp list

  val parseString : string -> sexp * ParsimonyStringInput.input
end
