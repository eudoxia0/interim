structure Backend :> BACKEND = struct
  datatype ctype = Bool
                 | UInt8
                 | Int8
                 | UInt16
                 | Int16
                 | UInt32
                 | Int32
                 | UInt64
                 | Int64

  datatype cparam = CParam of string * ctype

  datatype exp_cast = CConstBool of bool
                    | CConstInt of int
                    | CVar of string
                    | CBinop of AST.binop * exp_cast * exp_cast
                    | CCast of ctype * exp_cast
                    | CFuncall of string * exp_cast list

  datatype block_cast = CSeq of block_cast list
                      | CDeclare of ctype * string
                      | CAssign of string * exp_cast
                      | CCond of exp_cast * block_cast * block_cast

  datatype top_cast = CFunction of string * cparam list * ctype * block_cast

  val count = ref 0
  fun fresh s =
    let
    in
        count := !count + 1;
        s ^ (Int.toString (!count))
    end

  fun freshVar () = fresh "r"

  fun curVar () = CVar ("r" ^ (Int.toString (!count)))
  fun curVarStr () = let val (CVar s) = curVar () in s end

  fun convertType (Type.Unit) = Bool
    | convertType (Type.Bool) = Bool
    | convertType (Type.U8) = UInt8
    | convertType (Type.I8) = Int8
    | convertType (Type.U16) = UInt16
    | convertType (Type.I16) = Int16
    | convertType (Type.U32) = UInt32
    | convertType (Type.I32) = Int32
    | convertType (Type.U64) = UInt64
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
    fun convert TConstUnit = wrapConstant (CConstBool false) Bool
      | convert (TConstBool b) = wrapConstant (CConstBool b) Bool
      | convert (TConstInt (i, _)) = wrapConstant (CConstInt i) Int64
      | convert (TVar (s, t)) = wrapConstant (CVar s) (convertType t)
      | convert (TBinop (oper, a, b, t)) =
        let val a' = convert a
        in
            let val avar = curVar ()
            in
                let val b' = convert b
                in
                    let val bvar = curVar ()
                    in
                        CSeq [
                            a',
                            b',
                            wrapConstant (CBinop (oper, avar, bvar))
                                         (convertType t)
                        ]
                    end
                end
            end
        end
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
                        ]),
                  CDeclare (resType, freshVar ()),
                  CAssign (curVarStr (), CVar result)
                 ]
        end
      | convert (TCast (ty, a)) =
        let val a' = convert a
            and avar = curVar ()
            and res = freshVar ()
        in
            CSeq [
                a',
                CDeclare (convertType ty, res),
                CAssign (res, CCast (convertType ty, avar))
            ]
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

    fun defineFunction (Function.Function (name, params, rt)) tast =
      CFunction (name,
                 map (fn (Function.Param (n,t)) => CParam (n, convertType t)) params,
                 convertType rt,
                 convert tast)

    fun binopStr Add = "+"
      | binopStr Sub = "-"
      | binopStr Mul = "*"
      | binopStr Div = "/"
      | binopStr Eq = "="
      | binopStr LT = "<"
      | binopStr LEq = "<="
      | binopStr GT = ">"
      | binopStr GEq = ">="
  end

  fun renderType Bool = "bool"
    | renderType UInt8 = "uint8_t"
    | renderType Int8 = "int8_t"
    | renderType UInt16 = "uint16_t"
    | renderType Int16 = "int16_t"
    | renderType UInt32 = "uint32_t"
    | renderType Int32 = "int32_t"
    | renderType UInt64 = "uint64_t"
    | renderType Int64 = "int64_t"

  fun sepBy sep (string::nil) = string
    | sepBy sep strings = foldr (fn (a,b) => a ^ sep ^ b) "" strings

  fun pad n =
    if n > 0 then
        " " ^ (pad (n-1))
    else
        ""

  fun renderExp (CConstBool true) = "true"
    | renderExp (CConstBool false) = "false"
    | renderExp (CConstInt i) = (if i < 0 then "-" else "") ^ (Int.toString (abs i))
    | renderExp (CVar s) = s
    | renderExp (CBinop (oper, a, b)) = "(" ^ (renderExp a) ^ (binopStr oper) ^ (renderExp b) ^ ")"
    | renderExp (CCast (ty, a)) = "((" ^ (renderType ty) ^ ")(" ^ (renderExp a) ^ "))"
    | renderExp (CFuncall (f, args)) = f ^ "(" ^ (sepBy "," (map renderExp args)) ^ ")"

  fun renderBlock (CSeq l) = sepBy "\n" (map renderBlock l)
    | renderBlock (CDeclare (t, n)) = (renderType t) ^ " " ^ n ^ ";"
    | renderBlock (CAssign (n, v)) = n ^ " = " ^ (renderExp v) ^ ";"
    | renderBlock (CCond (t, c, a)) = "if (" ^ (renderExp t) ^ ") {\n" ^ (renderBlock c)
                                      ^ "\n} else { \n" ^ (renderBlock a) ^ "\n}"

  fun renderTop (CFunction (name, params, rt, body)) =
    (renderType rt) ^ " " ^ name ^ "(" ^ (sepBy "," (map renderParam params)) ^ ") {\n" ^ (renderBlock body) ^ "\n  return " ^ (renderExp (curVar ())) ^ ";\n}"
  and renderParam (CParam (n, t)) = (renderType t) ^ " " ^ n
end
