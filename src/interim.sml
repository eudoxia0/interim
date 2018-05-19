structure Interim :> INTERIM = struct
  open SymTab

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
                  let val c' = Compiler.compileString c s
                  in
                      print "Code:\n";
                      print (Compiler.compilerCode c');
                      print "\n";
                      repl' c'
                  end handle Fail s => print ("Error: " ^ s ^ "\n");
              repl' c
              end
          end
    in
        repl' Compiler.emptyCompiler
    end
end
