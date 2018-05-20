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
                 | RegionType

  datatype cparam = CParam of string * ctype

  datatype exp_cast = CConstBool of bool
                    | CConstInt of int
                    | CConstString of string
                    | CConstNull
                    | CVar of string
                    | CBinop of AST.binop * exp_cast * exp_cast
                    | CCast of ctype * exp_cast
                    | CDeref of exp_cast
                    | CAddressOf of exp_cast
                    | CSizeOf of ctype
                    | CStructInitializer of string * (string * exp_cast) list
                    | CStructAccess of exp_cast * string
                    | CAdjacent of exp_cast list
                    | CRaw of string

  datatype block_cast = CSeq of block_cast list
                      | CBlock of block_cast list
                      | CDeclare of ctype * string
                      | CAssign of exp_cast * exp_cast
                      | CCond of exp_cast * block_cast * block_cast
                      | CWhile of exp_cast * block_cast
                      | CFuncall of string option * string * exp_cast list

  datatype top_cast = CFunction of string * cparam list * ctype * block_cast * exp_cast
                    | CStructDef of string * (string * ctype) list

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
    | escapeChar #"'" = "_q"
    | escapeChar c = str c

  local
    open Type
  in
    fun convertIntType Unsigned Word8 = UInt8
      | convertIntType Signed   Word8 = Int8
      | convertIntType Unsigned Word16 = UInt16
      | convertIntType Signed   Word16 = Int16
      | convertIntType Unsigned Word32 = UInt32
      | convertIntType Signed   Word32 = Int32
      | convertIntType Unsigned Word64 = UInt64
      | convertIntType Signed   Word64 = Int64
  end

  fun convertType (Type.Unit) = Bool
    | convertType (Type.Bool) = Bool
    | convertType (Type.Int (s, w)) = convertIntType s w
    | convertType (Type.Str) = Pointer UInt8
    | convertType (Type.RawPointer t) = Pointer (convertType t)
    | convertType (Type.Record (n, _)) = Struct (escapeIdent n)
    | convertType (Type.RegionType _) = RegionType
    | convertType (Type.RegionPointer (t, _)) = Pointer (convertType t)
    | convertType (Type.NullablePointer (t, _)) = Pointer (convertType t)

  fun convertParamType (Type.PUnit) = Bool
    | convertParamType (Type.PBool) = Bool
    | convertParamType (Type.PInt (s, w)) = convertIntType s w
    | convertParamType (Type.PStr) = Pointer UInt8
    | convertParamType (Type.PRawPointer t) = Pointer (convertParamType t)
    | convertParamType (Type.PRecord (n, _)) = Struct (escapeIdent n)
    | convertParamType (Type.RegionParam _) = RegionType
    | convertParamType (Type.PRegionPointer (t, _)) = Pointer (convertParamType t)
    | convertParamType (Type.PNullablePointer (t, _)) = Pointer (convertParamType t)

  val unitConstant = CConstBool false

  local
    open Type
  in
    fun formatStringFor Unit n = [CConstString ("nil" ^ (newline n))]
      | formatStringFor Bool n = raise Fail "bool can't be printf'd"
      | formatStringFor (Int (Unsigned, Word8)) n = wrap "PRIu8" n
      | formatStringFor (Int (Signed,   Word8)) n = wrap "PRIi8" n
      | formatStringFor (Int (Unsigned, Word16)) n = wrap "PRIu16" n
      | formatStringFor (Int (Signed,   Word16)) n = wrap "PRIi16" n
      | formatStringFor (Int (Unsigned, Word32)) n = wrap "PRIu32" n
      | formatStringFor (Int (Signed,   Word32)) n = wrap "PRIi32" n
      | formatStringFor (Int (Unsigned, Word64)) n = wrap "PRIu64" n
      | formatStringFor (Int (Signed,   Word64)) n = wrap "PRIi64" n
      | formatStringFor Str n = [CConstString ("%s" ^ (newline n))]
      | formatStringFor (RawPointer _) n = [CConstString ("%p" ^ (newline n))]
      | formatStringFor _ _ = raise Fail "Records cannot be printf'd"
    and wrap s n = [CAdjacent [CConstString "%", CVar s, CConstString (newline n)]]
    and newline AST.Newline = "\\n"
      | newline AST.NoNewline = ""
  end

  fun regionName (Type.Region (id, name)) =
    "region_" ^ (escapeIdent name) ^ "_" ^ (Int.toString id)

  local
      open TAST
  in
    fun convert TConstUnit = (CSeq [], unitConstant)
      | convert (TConstBool b) = (CSeq [], CConstBool b)
      | convert (TConstInt (i, t)) = (CSeq [], CCast (convertType t, CConstInt i))
      | convert (TConstString s) = (CSeq [], CConstString s)
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
            (CSeq [vblock, CDeclare (ty, name), CAssign (CVar name, vval), bblock],
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
      | convert (TAddressOf (v, _)) =
        (CSeq [], CAddressOf (CVar v))
      | convert (TPrint (v, n)) =
        let val (vblock, vval) = convert v
            and ty = typeOf v
        in
            let val printer = if ty = Type.Bool then
                                  let val nl = (case n of
                                                   AST.Newline => CConstBool true
                                                 | AST.NoNewline => CConstBool false)
                                  in
                                      CFuncall (NONE, "interim_print_bool", [vval, nl])
                                  end
                              else
                                  CFuncall (NONE, "printf", (formatStringFor ty n) @ [vval])
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
      | convert (TWhile (t, b)) =
        let val (tblock, tval) = convert t
            and (bblock, _) = convert b
        in
            (CSeq [tblock, CWhile (tval, bblock)], unitConstant)
        end
      | convert (TLetRegion (r, b)) =
        let val (bblock, bval) = convert b
        in
            let val name = regionName r
            in
                (CSeq [CDeclare (RegionType, name),
                       CFuncall (NONE, "interim_region_create", [CAddressOf (CVar name)]),
                       bblock,
                       CFuncall (NONE, "interim_region_free", [CAddressOf (CVar name)])],
                 bval)
            end
        end
      | convert (TAllocate (r, v)) =
        let val (vblock, vval) = convert v
            and cr = CAddressOf (CVar (regionName r))
            and res = freshVar ()
            and cty = Pointer (convertType (typeOf v))
        in
            (CSeq [vblock,
                   CDeclare (cty, res),
                   CFuncall (SOME res, "interim_region_allocate", [cr, CSizeOf cty]),
                   CAssign (CDeref (CVar res), vval)],
             CVar res)
        end
      | convert (TNullableCase (p, var, nnc, nc, t)) =
        let val (pblock, pval) = convert p
            and (nncblock, nncval) = convert nnc
            and (ncblock, ncval) = convert nc
            and result = freshVar ()
            and resType = convertType t
        in
            (CSeq [
                  pblock,
                  CDeclare (resType, result),
                  CCond (CBinop (AST.NEq, pval, CConstNull),
                         CBlock [
                             CDeclare (convertType (typeOf p), escapeIdent var),
                             CAssign (CVar var, pval),
                             nncblock,
                             CAssign (CVar result, nncval)
                         ],
                         CBlock [
                             ncblock,
                             CAssign (CVar result, ncval)
                        ])
              ],
             CVar result)
        end
      | convert (TMakeRecord (ty, name, slots)) =
        let val args = map (fn (_, e) => convert e) slots
            and slot_names = map (fn (n, _) => n) slots
        in
            (CSeq (map (fn (b, _) => b) args),
             CStructInitializer (name,
                                 (ListPair.map (fn (name, v) => (name, v))
                                               (slot_names,
                                                map (fn (_, v) => v) args))))
        end
      | convert (TSlotAccess (r, s, _)) =
        let val (rblock, rval) = convert r
        in
            (rblock, CStructAccess (rval, s))
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
                     map (fn (Function.Param (n,t)) => CParam (n, convertParamType t)) params,
                     convertType rt,
                     block,
                     retval)
      end

    fun defineStruct name slots =
      CStructDef (name, map (fn (Type.Slot (n, t)) => (n, convertType t)) slots)
  end

  local
      open AST
  in
    fun binopStr Add = "+"
      | binopStr Sub = "-"
      | binopStr Mul = "*"
      | binopStr Div = "/"
      | binopStr Eq = "=="
      | binopStr NEq = "!="
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
    | renderType RegionType = "interim_region_t"

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
    | renderExp CConstNull = "NULL"
    | renderExp (CVar s) = (escapeIdent s)
    | renderExp (CBinop (oper, a, b)) =
      "(" ^ (renderExp a) ^ " " ^ (binopStr oper) ^ " " ^ (renderExp b) ^ ")"
    | renderExp (CCast (ty, a)) = "((" ^ (renderType ty) ^ ")(" ^ (renderExp a) ^ "))"
    | renderExp (CDeref e) = "*" ^ (renderExp e)
    | renderExp (CAddressOf e) = "&" ^ (renderExp e)
    | renderExp (CSizeOf t) = "sizeof(" ^ (renderType t) ^ ")"
    | renderExp (CStructInitializer (name, inits)) =
      "(("
      ^ (escapeIdent name)
      ^ ") { "
      ^ (String.concatWith ", " (map (fn (n, e) => "." ^ (escapeIdent n) ^ " = " ^ (renderExp e)) inits))
      ^ " })"
    | renderExp (CStructAccess (r, slot)) =
      (renderExp r)
      ^ "."
      ^ (escapeIdent slot)
    | renderExp (CAdjacent l) = String.concatWith " " (map renderExp l)
    | renderExp (CRaw s) = s

  fun renderBlock' d (CSeq l) = sepBy "\n" (map (renderBlock' d) l)
    | renderBlock' d (CBlock l) = "{\n" ^ (sepBy "\n" (map (renderBlock' d) l)) ^ "\n" ^ (pad (unindent d)) ^ "}"
    | renderBlock' d (CDeclare (t, n)) = (pad d) ^ (renderType t) ^ " " ^ (escapeIdent n) ^ ";"
    | renderBlock' d (CAssign (var, v)) = (pad d) ^ (renderExp var) ^ " = " ^ (renderExp v) ^ ";"
    | renderBlock' d (CCond (t, c, a)) = (pad d) ^ "if (" ^ (renderExp t) ^ ") " ^ (renderBlock' (indent d) c)
                                         ^ " else " ^ (renderBlock' (indent d) a)
    | renderBlock' d (CWhile (t, b)) =
      (pad d) ^ "while (" ^ (renderExp t) ^ ") {\n" ^ (renderBlock' (indent d) b) ^ "\n" ^ (pad d) ^ "}"
    | renderBlock' d (CFuncall (res, f, args)) =
      (pad d) ^ (renderRes res) ^ (escapeIdent f) ^ "(" ^ (sepBy "," (map renderExp args)) ^ ");"
  and renderRes (SOME res) = (escapeIdent res) ^ " = "
    | renderRes NONE = ""

  fun renderBlock b = renderBlock' (indent 0) b

  fun renderTop (CFunction (name, params, rt, body, retval)) =
    (renderType rt) ^ " " ^ (escapeIdent name) ^ "(" ^ (sepBy "," (map renderParam params)) ^ ") {\n" ^ (renderBlock body) ^ "\n  return " ^ (renderExp retval) ^ ";\n}"
    | renderTop (CStructDef (name, slots)) =
      "typedef struct { "
      ^ (String.concatWith " " (map (fn (n, t) => (renderType t) ^ " " ^ (escapeIdent n) ^ ";") slots))
      ^ " } " ^ (escapeIdent name) ^ ";\n"
  and renderParam (CParam (n, t)) = (renderType t) ^ " " ^ n
end
