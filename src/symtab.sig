signature SYMTAB = sig
  type 'a symtab

  val empty : 'a symtab
  val bind : (string * 'a) -> 'a symtab -> 'a symtab
  val lookup : string -> 'a symtab -> 'a option
end
