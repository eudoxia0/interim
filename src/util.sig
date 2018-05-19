signature UTIL = sig
  type path = string

  val readFileToString : path -> string
  val writeStringToFile : path -> string -> unit

  val member : ''a -> ''a list -> bool
end
