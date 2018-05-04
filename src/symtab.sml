structure SymTab :> SYMTAB = struct
  type 'a symtab = (string * 'a) list

  val empty = []

  fun bind (n, v) st = (n, v) :: st

  fun lookup s ((n,v)::xs) = if (n = s) then
                                 SOME v
                             else
                                 lookup s xs
    | lookup s nil = NONE
end
