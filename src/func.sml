structure Function :> FUNCTION = struct
  open SymTab

  datatype param = Param of string * Type.ty
  datatype func = Function of string * param list * Type.ty

  type fenv = func SymTab.symtab

  datatype mutability = Mutable
                      | Immutable

  datatype binding = Binding of string * Type.ty * mutability
  type stack = binding SymTab.symtab

  fun bindType (Binding (s, t, _)) = t

  fun funcName (Function (n, _, _)) = n

  fun funcRT (Function (_, _, r)) = r

  fun funcStack (Function (_, params, _)) =
    let fun toStack (Param (n,t)::rest) acc = bind (n, Binding (n, t, Immutable)) (toStack rest acc)
          | toStack nil acc = acc

    in
        toStack params empty
    end

  fun matchParams params types =
      if (length params <> length types) then
          raise Fail "Wrong argument count"
      else
          ListPair.all (fn (pt, at) => pt = at)
                       ((map (fn (Param (n, t)) => t) params),
                        types)
end
