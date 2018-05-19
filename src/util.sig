signature UTIL = sig
  type path = string

  val readFileToString : path -> string
  val writeStringToFile : path -> string -> unit

  val member : ''a -> ''a list -> bool
  val position : ''a -> ''a list -> int option
end
