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
                      let val ast = AST.parseToplevel sexp c
                      in
                          print "AST node\n"
                      end
                  end  handle Fail s => print ("Error: " ^ s ^ "\n");
                  repl' c
              end
          end
    in
        repl' (SymTab.empty)
    end
end
