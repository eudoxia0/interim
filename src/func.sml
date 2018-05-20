(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Interim.

    Interim is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Interim is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Interim.  If not, see <http://www.gnu.org/licenses/>.
*)

structure Function :> FUNCTION = struct
  open SymTab
  open Type

  datatype param = Param of string * pty
  datatype conc_param = ConcParam of string * Type.ty
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

  fun getRegion (name: string) (l: assignment list): region option =
    case List.find (fn (Assignment (n', _)) => name = n') l of
        SOME (Assignment (_, r)) => SOME r
      | NONE => NONE

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
    | matchType (PRecord (n, _)) (Record (n', _)) =
      if n = n' then
          emptyAssign
      else
          AssignFailure
    | matchType (RegionParam p) (RegionType (Region (i, p'))) =
      if p = p' then
          AssignList [Assignment (p, Region (i, p))]
      else
          AssignFailure
    | matchType (PRegionPointer (ty', p')) (RegionPointer (ty, (Region (i, p)))) =
      (case matchType ty' ty of
           AssignList l => if p = p' then
                               AssignList ((Assignment (p, Region (i, p))) :: l)
                           else
                               AssignFailure
         | AssignFailure => AssignFailure)
    | matchType _ _ = AssignFailure

  fun concretizeParam (Param (_, pty), ty): assignments =
    case (matchType pty ty) of
        AssignList l => AssignList l
      | AssignFailure => AssignFailure

  fun concatAssignments (AssignList l) (AssignList l') = AssignList (l @ l')
    | concatAssignments (AssignList _) AssignFailure = AssignFailure
    | concatAssignments AssignFailure (AssignList _) = AssignFailure
    | concatAssignments AssignFailure AssignFailure = AssignFailure

  fun concretize (params: param list) (types: ty list): assignments =
    let fun concat' (a::b::rest) = concatAssignments a (concat' (b::rest))
          | concat' [a] = a
          | concat' [] = AssignList []
    in
        concat' (ListPair.map concretizeParam (params, types))
    end

  fun subst (params: param list) (l: assignment list): conc_param list =
    map (substParam l) params
  and substParam a (Param (name, pty)) =
    ConcParam (name, substType pty a)
  and substType PUnit _ = Unit
    | substType PBool _ = Bool
    | substType (PInt i) _ = Int i
    | substType PStr _ = Str
    | substType (PRawPointer t) a = RawPointer (substType t a)
    | substType (PRecord d) _ = Record d
    | substType (RegionParam name) a =
      (case getRegion name a of
           SOME r => RegionType r
         | NONE => raise Fail "Region parameter not present in assignments")
    | substType (PRegionPointer (t, name)) a =
      (case getRegion name a of
           SOME r => RegionPointer (substType t a, r)
         | NONE => raise Fail "Region parameter not present in assignments")
    | substType (PNullablePointer (t, name)) a =
      case getRegion name a of
          SOME r => NullablePointer (substType t a, r)
        | NONE => raise Fail "Region parameter not present in assignments"

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
                                          subst params l
                                  end
                              end
            | AssignFailure => raise Fail "Argument types did not match parameter types"

  fun regionParams PUnit = []
    | regionParams PBool = []
    | regionParams (PInt _) = []
    | regionParams PStr = []
    | regionParams (PRawPointer t) = regionParams t
    | regionParams (PRecord _) = []
    | regionParams (RegionParam name) = [name]
    | regionParams (PRegionPointer (_, name)) = [name]
    | regionParams (PNullablePointer (_, name)) = [name]

  fun getIndex elem list =
    case (Util.position elem list) of
        SOME p => p
      | NONE => raise Fail "Shouldn't happen"

  fun forciblyConcretizeType' PUnit _ = Unit
    | forciblyConcretizeType' PBool _ = Bool
    | forciblyConcretizeType' (PInt i) _ = Int i
    | forciblyConcretizeType' PStr _ = Str
    | forciblyConcretizeType' (PRawPointer t) e = RawPointer (forciblyConcretizeType' t e)
    | forciblyConcretizeType' (PRecord d) _ = Record d
    | forciblyConcretizeType' (RegionParam name) e = RegionType (Region (getIndex name e, name))
    | forciblyConcretizeType' (PRegionPointer (ty, name)) e =
      RegionPointer (forciblyConcretizeType' ty e, (Region (getIndex name e, name)))
    | forciblyConcretizeType' (PNullablePointer (ty, name)) e =
      NullablePointer (forciblyConcretizeType' ty e, (Region (getIndex name e, name)))

  fun forciblyConcretizeType pt =
    forciblyConcretizeType' pt (regionParams pt)

  fun toStack (Function (_, params, _)) =
    let fun toStack' (Param (n,t)::rest) acc = bind (n, Binding (n, forciblyConcretizeType t, Immutable))
                                                        (toStack' rest acc)
          | toStack' nil acc = acc

    in
        toStack' params empty
    end
end
