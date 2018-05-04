structure Interim :> INTERIM = struct
  type compiler = Type.tenv

  fun readUntilBlank () =
    case (TextIO.inputLine TextIO.stdIn) of
        (SOME s) => if s = "\n" then
                        ""
                    else
                        (s ^ (readUntilBlank ()))
      | NONE => OS.Process.terminate OS.Process.success

  fun repl () =
    let fun repl' c =
          let
          in
              print "> ";
              let val s = readUntilBlank ()
              in
                  let val sexp = Parser.parseString s
                  in
                      case sexp of
                          (SOME n) => (case (AST.parseToplevel n c) of
                                           (SOME a) => print "AST node\n"
                                         | NONE => print "Bad sexp->AST parse\n")
                        | _ => print "Bad parse\n"
                  end;
                  repl' c
              end
          end
    in
        repl' (SymTab.empty)
    end
end
