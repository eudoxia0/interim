structure Backend :> BACKEND = struct
  datatype ctype = Int64

  datatype param = Param of string * ctype

  datatype top_ast = Function of string * param list * ctype

  fun convert a = raise Fail "derp"
end
