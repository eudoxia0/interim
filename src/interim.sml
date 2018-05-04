structure Interim :> INTERIM = struct
  type compiler = Type.tenv * AST.fenv

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
                               (AST.Defun (func, ast)) => print "Defined a function\n")
                      end
                  end  handle Fail s => print ("Error: " ^ s ^ "\n");
                  repl' (tenv, fenv)
              end
          end
    in
        repl' (SymTab.empty, SymTab.empty)
    end
end
