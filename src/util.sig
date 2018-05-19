signature UTIL = sig
  type path = string

  val readFileToString : path -> string
  val writeStringToFile : path -> string -> unit
end
