structure Interim :> INTERIM = struct
  open SymTab

  type compiler = Type.tenv * Function.fenv

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

  fun readUntilBlank () =
    case (TextIO.inputLine TextIO.stdIn) of
        (SOME s) => if s = "\n" then
                        ""
                    else
                        (s ^ (readUntilBlank ()))
      | NONE => OS.Process.terminate OS.Process.success

  fun repl () =
    let fun repl' (tenv, fenv) =
          let
          in
              print "> ";
              let val s = readUntilBlank ()
              in
                  let val sexp = Parser.parseString s
                  in
                      let val ast = AST.parseToplevel sexp tenv
                      in
                          (case ast of
                               (AST.Defun (func, ast)) =>
                               let val fenv' = bind (Function.funcName func, func) fenv
                               in
                                   let val tast = TAST.augment ast (Function.funcStack func) tenv fenv'
                                   in
                                       if (TAST.typeOf tast) <> Function.funcRT func then
                                           raise Fail "Return type does not match type of body"
                                       else
                                           let val code = Backend.defineFunction func tast
                                           in
                                               print "Code:\n";
                                               print prelude;
                                               print "\n";
                                               print (Backend.renderTop code);
                                               print "\n";
                                               repl' (tenv, fenv')
                                           end
                                   end
                               end
                             | (AST.CInclude s) =>
                               let
                               in
                                   repl' (tenv, fenv)
                               end)
                      end
                  end  handle Fail s => print ("Error: " ^ s ^ "\n");
                  repl' (tenv, fenv)
              end
          end
    in
        repl' (empty, empty)
    end
end
