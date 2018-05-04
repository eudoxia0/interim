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
                | CFuncall of string * cast list

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

  fun wrapConstant c t =
    let val result = freshVar ()
    in
        CSeq [
            CDeclare (t, result),
            CAssign (result, c)
        ]
    end

  local
      open AST
  in
    fun convert (TConstBool b) = wrapConstant (CConstBool b) Bool
      | convert (TConstInt (i, _)) = wrapConstant (CConstInt i) Int64
      | convert (TVar (s, t)) = wrapConstant (CVar s) (convertType t)
      | convert (TBinop (oper, a, b, t)) = wrapConstant (CBinop (oper, convert a, convert b))
                                                        (convertType t)
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
      | convert (TFuncall (f, args, rt)) =
        let val args' = map (fn a => (convert a, curVar ())) args
            and rt' = convertType rt
            and res = freshVar ()
        in
            CSeq ((map (fn (c, v) => c) args')
                  @
                  [CDeclare (rt', res),
                   CAssign (res, CFuncall (f, map (fn (c, v) => v) args'))])
        end

    fun convertTop a = raise Fail "derp"
  end
end
