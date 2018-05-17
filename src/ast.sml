structure AST :> AST = struct
  open SymTab

  datatype binop = Add
                 | Sub
                 | Mul
                 | Div
                 | Eq
                 | LT
                 | LEq
                 | GT
                 | GEq

  datatype ast = ConstUnit
               | ConstBool of bool
               | ConstInt of int
               | ConstString of string
               | Var of string
               | Binop of binop * ast * ast
               | Cond of ast * ast * ast
               | Cast of Type.ty * ast
               | Progn of ast list
               | Let of string * ast * ast
               | Assign of string * ast
               | NullPtr of Parser.sexp
               | Load of ast
               | Store of ast * ast
               | Malloc of Parser.sexp * ast
               | Free of ast
               | AddressOf of string
               | Print of ast
               | CEmbed of Parser.sexp * string
               | CCall of string * Parser.sexp * ast list
               | While of ast * ast
               | Funcall of string * ast list

  datatype top_ast = Defun of Function.func * ast

  local
    open Parser
  in
    fun parse (Integer i) _ = ConstInt i
      | parse (String s) _ = ConstString s
      | parse (Symbol "nil") _ = ConstUnit
      | parse (Symbol "true") _ = ConstBool true
      | parse (Symbol "false") _ = ConstBool false
      | parse (Symbol s) _ = Var s
      | parse (SList [Symbol "+", a, b]) e = Binop (Add, parse a e, parse b e)
      | parse (SList [Symbol "-", a, b]) e = Binop (Sub, parse a e, parse b e)
      | parse (SList [Symbol "*", a, b]) e = Binop (Mul, parse a e, parse b e)
      | parse (SList [Symbol "/", a, b]) e = Binop (Div, parse a e, parse b e)
      | parse (SList [Symbol "=", a, b]) e = Binop (Eq, parse a e, parse b e)
      | parse (SList [Symbol "<", a, b]) e = Binop (LT, parse a e, parse b e)
      | parse (SList [Symbol "<=", a, b]) e = Binop (LEq, parse a e, parse b e)
      | parse (SList [Symbol ">", a, b]) e = Binop (GT, parse a e, parse b e)
      | parse (SList [Symbol ">=", a, b]) e = Binop (GEq, parse a e, parse b e)
      | parse (SList [Symbol "if", t, c, a]) e = Cond (parse t e, parse c e, parse a e)
      | parse (SList [Symbol "the", t, a]) e = Cast (Type.parseTypeSpecifier t e, parse a e)
      | parse (SList ((Symbol "progn")::rest)) e = Progn (map (fn a => parse a e) rest)
      | parse (SList ((Symbol "let")::(SList [SList [Symbol var, v]])::body)) e =
        Let (var, parse v e, Progn (map (fn a => parse a e) body))
      | parse (SList ((Symbol "let")::(SList ((SList [Symbol var, v])::rest))::body)) e =
        let val exp = SList [Symbol "let", SList [SList [Symbol var, v]],
                             SList ((Symbol "let")::(SList rest)::body)]
        in
            parse exp e
        end
      | parse (SList ((Symbol "let")::(SList nil)::body)) e =
        Progn (map (fn a => parse a e) body)
      | parse (SList [Symbol "<-", Symbol var, v]) e = Assign (var, parse v e)
      | parse (SList [Symbol "nullptr", t]) _ = NullPtr t
      | parse (SList [Symbol "load", v]) e = Load (parse v e)
      | parse (SList [Symbol "store", p, v]) e = Store (parse p e, parse v e)
      | parse (SList [Symbol "malloc", t, c]) e = Malloc (t, parse c e)
      | parse (SList [Symbol "free", p]) e = Free (parse p e)
      | parse (SList [Symbol "address-of", Symbol v]) _ = AddressOf v
      | parse (SList [Symbol "print", v]) e = Print (parse v e)
      | parse (SList [Symbol "c/embed", t, String s]) _ = CEmbed (t, s)
      | parse (SList [Symbol "c/embed", _, _]) _ = raise Fail "Bad c/embed form"
      | parse (SList (Symbol "c/call" :: String n :: t :: args)) e = CCall (n, t, map (fn a => parse a e) args)
      | parse (SList (Symbol "while" :: t :: body)) e = While (parse t e, Progn (map (fn c => parse c e) body))
      | parse (SList ((Symbol s)::rest)) e = Funcall (s, map (fn a => parse a e) rest)
      | parse _ _ = raise Fail "Bad expression"

    fun parseParam (SList [Symbol n, t]) e = Function.Param (n, Type.parseTypeSpecifier t e)
      | parseParam _ _ = raise Fail "Bad parameter"

    fun parseToplevel (SList [Symbol "defun", Symbol name, SList params, rt, body]) e =
      Defun (Function.Function (name,
                                map (fn p => parseParam p e) params,
                                Type.parseTypeSpecifier rt e),
             parse body e)
      | parseToplevel _ _ = raise Fail "Bad toplevel node"
  end
end
