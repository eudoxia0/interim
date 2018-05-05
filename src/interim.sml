structure Interim :> INTERIM = struct
  open SymTab

  type compiler = Type.tenv * Function.fenv

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
                                   let val tast = AST.augment ast (Function.funcStack func) tenv fenv'
                                   in
                                       if (AST.typeOf tast) <> Function.funcRT func then
                                           raise Fail "Return type does not match type of body"
                                       else
                                           let val code = Backend.defineFunction func tast
                                           in
                                               print "Code:\n";
                                               print (Backend.renderTop code);
                                               repl' (tenv, fenv')
                                           end
                                   end
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
