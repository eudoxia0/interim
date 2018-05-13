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
                      | CBlock of block_cast list
                      | CDeclare of ctype * string
                      | CAssign of string * exp_cast
                      | CCond of exp_cast * block_cast * block_cast

  datatype top_cast = CFunction of string * cparam list * ctype * block_cast * exp_cast

  val count = ref 0
  fun fresh s =
    let
    in
        count := !count + 1;
        s ^ (Int.toString (!count))
    end

  fun freshVar () = fresh "r"

  fun escapeIdent s = String.concat (map escapeChar (String.explode s))
  and escapeChar #"+" = "_p"
    | escapeChar #"-" = "__"
    | escapeChar #"*" = "_m"
    | escapeChar #"/" = "_d"
    | escapeChar #">" = "_g"
    | escapeChar #"<" = "_l"
    | escapeChar #"=" = "_e"
    | escapeChar c = str c

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

  local
      open TAST
  in
    fun convert TConstUnit = (CSeq [], CConstBool false)
      | convert (TConstBool b) = (CSeq [], CConstBool b)
      | convert (TConstInt (i, _)) = (CSeq [], CConstInt i)
      | convert (TVar (s, t)) = (CSeq [], CVar s)
      | convert (TBinop (oper, a, b, t)) =
        let val (ablock, aval) = convert a
            and (bblock, bval) = convert b
        in
            (CSeq [
                  ablock,
                  bblock
              ],
             (CBinop (oper, aval, bval)))
        end
      | convert (TCond (t, c, a, _)) =
        let val (tblock, tval) = convert t
            and (cblock, cval) = convert c
            and (ablock, aval) = convert a
            and result = freshVar ()
            and resType = convertType (TAST.typeOf c)
        in
            (CSeq [
                  tblock,
                  CDeclare (resType, result),
                  CCond (tval,
                         CBlock [
                             cblock,
                             CAssign (result, cval)
                         ],
                         CBlock [
                             ablock,
                             CAssign (result, aval)
                        ])
              ],
             CVar result)
        end
      | convert (TCast (ty, a)) =
        let val (ablock, aval) = convert a
        in
            (ablock, CCast (convertType ty, aval))
        end
      | convert (TProgn exps) =
        let val exps' = map convert exps
        in
            if (length exps = 0) then
                (CSeq [], CConstBool false)
            else
                (CSeq (map (fn (b, _) => b) exps'),
                 let val (_, v) = List.last exps' in v end)
        end
      | convert (TLet (name, v, b)) =
        let val (vblock, vval) = convert v
            and ty = convertType (typeOf v)
            and (bblock, bval) = convert b
        in
            (CSeq ([vblock] @ [CDeclare (ty, name), CAssign (name, vval)] @ [bblock]),
             bval)
        end
      | convert (TFuncall (f, args, rt)) =
        let val args' = map (fn a => convert a) args
            and rt' = convertType rt
        in
            (CSeq (map (fn (b, v) => b) args'),
             CFuncall (f, map (fn (b, v) => v) args'))
        end

    fun defineFunction (Function.Function (name, params, rt)) tast =
      let val (block, retval) = convert tast
      in
          CFunction (name,
                     map (fn (Function.Param (n,t)) => CParam (n, convertType t)) params,
                     convertType rt,
                     block,
                     retval)
      end
  end

  local
      open AST
  in
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

  local
      open Substring
  in
    fun sepBy sep strings = trimWhitespace (String.concatWith sep strings)
    and trimWhitespace s = string (dropl (fn c => c = #"\n") (full s))
  end

  fun pad n =
    if n > 0 then
        " " ^ (pad (n-1))
    else
        ""

  val indentation = 2
  fun indent d = d + indentation
  fun unindent d = d - indentation

  fun renderExp (CConstBool true) = "true"
    | renderExp (CConstBool false) = "false"
    | renderExp (CConstInt i) = (if i < 0 then "-" else "") ^ (Int.toString (abs i))
    | renderExp (CVar s) = (escapeIdent s)
    | renderExp (CBinop (oper, a, b)) =
      "(" ^ (renderExp a) ^ " " ^ (binopStr oper) ^ " " ^ (renderExp b) ^ ")"
    | renderExp (CCast (ty, a)) = "((" ^ (renderType ty) ^ ")(" ^ (renderExp a) ^ "))"
    | renderExp (CFuncall (f, args)) = (escapeIdent f) ^ "(" ^ (sepBy "," (map renderExp args)) ^ ")"

  fun renderBlock' d (CSeq l) = sepBy "\n" (map (renderBlock' d) l)
    | renderBlock' d (CBlock l) = "{\n" ^ (sepBy "\n" (map (renderBlock' d) l)) ^ "\n" ^ (pad (unindent d)) ^ "}"
    | renderBlock' d (CDeclare (t, n)) = (pad d) ^ (renderType t) ^ " " ^ n ^ ";"
    | renderBlock' d (CAssign (n, v)) = (pad d) ^ n ^ " = " ^ (renderExp v) ^ ";"
    | renderBlock' d (CCond (t, c, a)) = (pad d) ^ "if (" ^ (renderExp t) ^ ") " ^ (renderBlock' (indent d) c)
                                         ^ " else " ^ (renderBlock' (indent d) a)

  fun renderBlock b = renderBlock' (indent 0) b

  fun renderTop (CFunction (name, params, rt, body, retval)) =
    (renderType rt) ^ " " ^ (escapeIdent name) ^ "(" ^ (sepBy "," (map renderParam params)) ^ ") {\n" ^ (renderBlock body) ^ "\n  return " ^ (renderExp retval) ^ ";\n}"
  and renderParam (CParam (n, t)) = (renderType t) ^ " " ^ n
end
