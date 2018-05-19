structure Compiler :> COMPILER = struct
  open SymTab

  type compiler = Type.tenv * Function.fenv * string

  val prelude = String.concatWith "\n" [
          "#include <stdbool.h>",
          "#include <inttypes.h>",
          "#include <stdio.h>",
          "#include <stdlib.h>",
          "",
          "int interim_print_bool(bool v, bool nl) {",
          "  if (nl) {",
          "    return printf(v ? \"true\\n\" : \"false\\n\");",
          "  } else {",
          "    return printf(v ? \"true\" : \"false\");",
          "  }",
          "}",
          ""
      ]
  val emptyCompiler = (empty, empty, prelude)

  fun compilerTypeEnv (t, _, _) = t
  fun compilerCode (_, _, c) = c

  fun compileAST (tenv, fenv, code) ast =
    (case ast of
         (AST.Defun (func, ast)) =>
         let val fenv' = bind (Function.funcName func, func) fenv
         in
             let val tast = TAST.augment ast (Function.funcStack func) tenv fenv'
             in
                 if (TAST.typeOf tast) <> Function.funcRT func then
                     raise Fail "Return type does not match type of body"
                 else
                     let val code' = Backend.defineFunction func tast
                     in
                         (tenv, fenv', code ^ (Backend.renderTop code'))
                     end
             end
         end
       | (AST.CInclude s) =>
         let val incl = "\n#include <" ^ s ^ ">\n\n"
         in
             (tenv, fenv, code ^ incl)
         end)

  fun compileString c s =
    let val (sexp, _) = Parser.parseString s
    in
        compileAST c (AST.parseToplevel sexp (compilerTypeEnv c))
    end

  fun compileForms c (form::rest) = let val c' = compileAST c (AST.parseToplevel form (compilerTypeEnv c))
                                    in
                                        compileForms c' rest
                                    end
    | compileForms c nil = c

  fun compileFile c path =
    let val code = "(" ^ (Util.readFileToString path) ^ ")"
    in
        case (Parser.parseString code) of
            (Parser.SList l, _) => compileForms c l
          | _ => raise Fail "Impossible"
    end
end