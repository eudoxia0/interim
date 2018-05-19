structure Function :> FUNCTION = struct
  open SymTab
  open Type

  datatype param = Param of string * pty
  datatype func = Function of string * param list * ty

  type fenv = func SymTab.symtab

  datatype mutability = Mutable
                      | Immutable

  datatype binding = Binding of string * ty * mutability
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

  datatype assignment = Assignment of string * region

  type assignments = assignment list

  fun concretize (params: param list) (types: ty list): assignments =
    List.concat (ListPair.map concretizeParam (params, types))
  and concretizeParam (Param (name, pty), ty): assignments = []

  fun matchType Unit PUnit = SOME Unit
    | matchType Bool PBool = SOME Bool
    | matchType (Int (s, w)) (PInt (s', w')) =
      if (s = s') andalso (w = w') then
          SOME (Int (s, w))
      else
          NONE
    | matchType Str PStr = SOME Str
    | matchType (RawPointer t) (PRawPointer t') =
      (case (matchType t t') of
           SOME t => SOME (RawPointer t)
         | NONE => NONE)
    | matchType (Record slots) _ = raise Fail "RECORDS NOT SUPPORTED"
    | matchType (RegionType (Region (i, s))) (RegionParam s') =
      if s = s' then
          SOME (RegionType (Region (i, s)))
      else
          NONE

  fun matchParams params types =
      if (length params <> length types) then
          raise Fail "Wrong argument count"
      else
          ListPair.all (fn (pt, at) => pt = at)
                       ((map (fn (Param (n, t)) => t) params),
                        types)
end
