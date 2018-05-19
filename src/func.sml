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

  datatype assignment = Assignment of string * region

  datatype assignments = AssignList of assignment list
                       | AssignFailure

  val emptyAssign = AssignList []

  fun matchType PUnit Unit = emptyAssign
    | matchType PBool Bool = emptyAssign
    | matchType (PInt (s, w)) (Int (s', w')) =
      if (s = s') andalso (w = w') then
          emptyAssign
      else
          AssignFailure
    | matchType PStr Str = emptyAssign
    | matchType (PRawPointer t) (RawPointer t') = matchType t t'
    | matchType _ (Record _) = raise Fail "RECORDS NOT SUPPORTED"
    | matchType (RegionParam p) (RegionType (Region (i, p'))) =
      if p = p' then
          AssignList [Assignment (p, Region (i, p))]
      else
          AssignFailure

  fun concretizeParam (Param (_, pty), ty): assignments =
    case (matchType pty ty) of
        AssignList l => AssignList l
      | AssignFailure => AssignFailure

  fun concatAssignments ((AssignList l), (AssignList l')) = AssignList (l @ l')
    | concatAssignments ((AssignList _), AssignFailure) = AssignFailure
    | concatAssignments (AssignFailure, (AssignList _)) = AssignFailure
    | concatAssignments (AssignFailure, AssignFailure) = AssignFailure

  fun concretize (params: param list) (types: ty list): assignments =
    List.foldl concatAssignments AssignFailure (ListPair.map concretizeParam (params, types))

  fun matchParams params types =
      if (length params <> length types) then
          raise Fail "Wrong argument count"
      else
          case concretize params types of
              AssignList l => let val rpnames = map (fn (Assignment (n, _)) => n) l
                              in
                                  let val nameset = Set.fromList rpnames
                                  in
                                      if (length rpnames) <> (Set.size nameset) then
                                          raise Fail "There are duplicate assignments to region parameters"
                                      else
                                          raise Fail "TO BE DONE"
                                  end
                              end
            | AssignFailure => raise Fail "Argument types did not match parameter types"

  fun funcStack (Function (_, params, _)) =
    let fun toStack (Param (n,t)::rest) acc = bind (n, Binding (n, t, Immutable)) (toStack rest acc)
          | toStack nil acc = acc

    in
        toStack params empty
    end

end
