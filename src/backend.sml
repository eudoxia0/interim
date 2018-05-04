structure Backend :> BACKEND = struct
  datatype ctype = Bool
                 | Int64

  datatype cparam = CParam of string * ctype

  datatype top_cast = CFunction of string * cparam list * ctype

  datatype cast = CConstBool of bool
                | CConstInt of int
                | CVar of string
                | CBinop of AST.binop * cast * cast
                | CSeq of cast list
                | CDeclare of ctype * string
                | CAssign of string * cast
                | CCond of cast * cast * cast

  val count = ref 0
  fun fresh s =
    let val cur = !count
    in
        count := cur + 1;
        s ^ (Int.toString cur)
    end

  fun freshVar () = fresh "r"

  fun curVar () = CVar ("r" ^ (Int.toString (!count)))

  fun convertType (Type.Unit) = Bool
    | convertType (Type.Bool) = Bool
    | convertType (Type.I64) = Int64

  local
      open AST
  in
    fun convert (TConstBool b) = CConstBool b
      | convert (TConstInt (i, _)) = CConstInt i
      | convert (TVar (s, _)) = CVar s
      | convert (TBinop (oper, a, b, _)) = CBinop (oper, convert a, convert b)
      | convert (TCond (t, c, a, _)) =
        let val result = freshVar ()
            and resType = convertType (AST.typeOf c)
        in
            CSeq [CDeclare (resType, result),
                  convert t,
                  CCond (curVar (),
                         CSeq [
                             convert c,
                             CAssign (result, curVar ())
                         ],
                         CSeq [
                             convert a,
                             CAssign (result, curVar ())
                         ])]
        end
      | convert _ = raise Fail "derp"
    fun convertTop a = raise Fail "derp"
  end
end
