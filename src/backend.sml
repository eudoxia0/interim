structure Backend :> BACKEND = struct
  datatype ctype = Bool
                 | Int64

  datatype cparam = CParam of string * ctype

  datatype exp_cast = CConstBool of bool
                    | CConstInt of int
                    | CVar of string
                    | CBinop of AST.binop * exp_cast * exp_cast
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
                        ])
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

  fun typeName Bool = "bool"
    | typeName Int64 = "int64_t"

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
    | renderExp (CFuncall (f, args)) = f ^ "(" ^ (sepBy "," (map renderExp args)) ^ ")"

  fun renderBlock (CSeq l) = sepBy "\n" (map renderBlock l)
    | renderBlock (CDeclare (t, n)) = (typeName t) ^ " " ^ n ^ ";"
    | renderBlock (CAssign (n, v)) = n ^ " = " ^ (renderExp v) ^ ";"
    | renderBlock (CCond (t, c, a)) = "if (" ^ (renderExp t) ^ ") {\n" ^ (renderBlock c)
                                      ^ "\n} else { \n" ^ (renderBlock a) ^ "\n}"

  fun renderTop (CFunction (name, params, rt, body)) =
    (typeName rt) ^ " " ^ name ^ "(" ^ (sepBy "," (map renderParam params)) ^ ") {\n" ^ (renderBlock body) ^ "\n  return " ^ (renderExp (curVar ())) ^ ";\n}"
  and renderParam (CParam (n, t)) = (typeName t) ^ " " ^ n
end
