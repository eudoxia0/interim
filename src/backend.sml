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
                 | Pointer of ctype
                 | Struct of string

  datatype cparam = CParam of string * ctype

  datatype exp_cast = CConstBool of bool
                    | CConstInt of int
                    | CConstString of string
                    | CConstNull
                    | CVar of string
                    | CBinop of AST.binop * exp_cast * exp_cast
                    | CCast of ctype * exp_cast
                    | CDeref of exp_cast
                    | CSizeOf of ctype
                    | CAdjacent of exp_cast list
                    | CRaw of string

  datatype block_cast = CSeq of block_cast list
                      | CBlock of block_cast list
                      | CDeclare of ctype * string
                      | CAssign of exp_cast * exp_cast
                      | CCond of exp_cast * block_cast * block_cast
                      | CFuncall of string option * string * exp_cast list

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
    | convertType (Type.RawPointer t) = Pointer (convertType t)
    | convertType (Type.Record (n, _)) = Struct (escapeIdent n)

  val unitConstant = CConstBool false

  local
      open Type
  in
    fun formatStringFor Unit = [CConstString "nil"]
      | formatStringFor Bool = raise Fail "bool can't be printf'd"
      | formatStringFor U8 = wrap "PRIu8"
      | formatStringFor I8 = wrap "PRIi8"
      | formatStringFor U16 = wrap "PRIu16"
      | formatStringFor I16 = wrap "PRIi16"
      | formatStringFor U32 = wrap "PRIu32"
      | formatStringFor I32 = wrap "PRIi32"
      | formatStringFor U64 = wrap "PRIu64"
      | formatStringFor I64 = wrap "PRIi64"
      | formatStringFor (RawPointer _) = [CConstString "%p"]
      | formatStringFor _ = raise Fail "Records cannot be printf'd"
    and wrap s = [CAdjacent [CConstString "%", CVar s]]
  end

  local
      open TAST
  in
    fun convert TConstUnit = (CSeq [], unitConstant)
      | convert (TConstBool b) = (CSeq [], CConstBool b)
      | convert (TConstInt (i, t)) = (CSeq [], CCast (convertType t, CConstInt i))
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
                             CAssign (CVar result, cval)
                         ],
                         CBlock [
                             ablock,
                             CAssign (CVar result, aval)
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
                (CSeq [], unitConstant)
            else
                (CSeq (map (fn (b, _) => b) exps'),
                 let val (_, v) = List.last exps' in v end)
        end
      | convert (TLet (name, v, b)) =
        let val (vblock, vval) = convert v
            and ty = convertType (typeOf v)
            and (bblock, bval) = convert b
        in
            (CSeq ([vblock] @ [CDeclare (ty, name), CAssign (CVar name, vval)] @ [bblock]),
             bval)
        end
      | convert (TAssign (var, v)) =
        let val (vblock, vval) = convert v
        in
            (CSeq [vblock, CAssign (CVar var, vval)], vval)
        end
      | convert (TNullPtr _) = (CSeq [], CConstNull)
      | convert (TLoad (e, _)) =
        let val (eblock, eval) = convert e
        in
            (eblock, CDeref eval)
        end
      | convert (TStore (p, v)) =
        let val (pblock, pval) = convert p
            and (vblock, vval) = convert v
        in
            (CSeq [pblock, vblock, CAssign ((CDeref pval), vval)], vval)
        end
      | convert (TMalloc (t, c)) =
        let val (cblock, cval) = convert c
            and ty = convertType t
            and res = freshVar ()
        in
            let val sizecalc = CBinop (AST.Mul, cval, CSizeOf ty)
            in
                (CSeq [cblock, CDeclare (Pointer ty, res), CFuncall (SOME res, "malloc", [sizecalc])],
                 CCast (Pointer ty, CVar res))
            end
        end
      | convert (TFree p) =
        let val (pblock, pval) = convert p
        in
            (CSeq [pblock, CFuncall (NONE, "free", [pval])], unitConstant)
        end
      | convert (TPrint v) =
        let val (vblock, vval) = convert v
            and ty = typeOf v
        in
            let val printer = if ty = Type.Bool then
                                  CFuncall (NONE, "interim_print_bool", [vval])
                              else
                                  CFuncall (NONE, "printf", (formatStringFor ty) @ [vval])
            in
                (CSeq [vblock, printer],
                 unitConstant)
            end
        end
      | convert (TCEmbed (t, s)) =
        (CSeq [], CCast (convertType t, CRaw s))
      | convert (TCCall (f, t, args)) =
        let val args' = map (fn a => convert a) args
            and t' = convertType t
        in
             let val blocks = map (fn (b, _) => b) args'
                 and argvals = map (fn (_, v) => v) args'
             in
                 if t = Type.Unit then
                     (CSeq (blocks @ [CFuncall (NONE, f, argvals)]),
                      unitConstant)
                 else
                     let val res = freshVar ()
                     in
                         (CSeq (blocks @ [CDeclare (t', res), CFuncall (SOME res, f, argvals)]),
                          CVar res)
                     end
             end
        end
      | convert (TFuncall (f, args, rt)) =

        let val args' = map (fn a => convert a) args
            and rt' = convertType rt
            and res = freshVar ()
        in
            let val blocks = map (fn (b, _) => b) args'
                and argvals = map (fn (_, v) => v) args'
            in
                (CSeq (blocks @ [CDeclare (rt', res), CFuncall (SOME res, f, argvals)]),
                 CVar res)
            end
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
      | binopStr Eq = "=="
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
    | renderType (Pointer t) = (renderType t) ^ "*"
    | renderType (Struct n) = n

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
    | renderExp (CConstString s) =
      let fun tr #"\"" = "\\\""
            | tr c = str c
      in
          "\"" ^ (String.translate tr s) ^ "\""
      end
    | renderExp CConstNull = "null"
    | renderExp (CVar s) = (escapeIdent s)
    | renderExp (CBinop (oper, a, b)) =
      "(" ^ (renderExp a) ^ " " ^ (binopStr oper) ^ " " ^ (renderExp b) ^ ")"
    | renderExp (CCast (ty, a)) = "((" ^ (renderType ty) ^ ")(" ^ (renderExp a) ^ "))"
    | renderExp (CDeref e) = "*" ^ (renderExp e)
    | renderExp (CSizeOf t) = "sizeof(" ^ (renderType t) ^ ")"
    | renderExp (CAdjacent l) = String.concatWith " " (map renderExp l)
    | renderExp (CRaw s) = s

  fun renderBlock' d (CSeq l) = sepBy "\n" (map (renderBlock' d) l)
    | renderBlock' d (CBlock l) = "{\n" ^ (sepBy "\n" (map (renderBlock' d) l)) ^ "\n" ^ (pad (unindent d)) ^ "}"
    | renderBlock' d (CDeclare (t, n)) = (pad d) ^ (renderType t) ^ " " ^ n ^ ";"
    | renderBlock' d (CAssign (var, v)) = (pad d) ^ (renderExp var) ^ " = " ^ (renderExp v) ^ ";"
    | renderBlock' d (CCond (t, c, a)) = (pad d) ^ "if (" ^ (renderExp t) ^ ") " ^ (renderBlock' (indent d) c)
                                         ^ " else " ^ (renderBlock' (indent d) a)
    | renderBlock' d (CFuncall (res, f, args)) =
      (pad d) ^ (renderRes res) ^ (escapeIdent f) ^ "(" ^ (sepBy "," (map renderExp args)) ^ ");"
  and renderRes (SOME res) = (escapeIdent res) ^ " = "
    | renderRes NONE = ""

  fun renderBlock b = renderBlock' (indent 0) b

  fun renderTop (CFunction (name, params, rt, body, retval)) =
    (renderType rt) ^ " " ^ (escapeIdent name) ^ "(" ^ (sepBy "," (map renderParam params)) ^ ") {\n" ^ (renderBlock body) ^ "\n  return " ^ (renderExp retval) ^ ";\n}"
  and renderParam (CParam (n, t)) = (renderType t) ^ " " ^ n
end
