structure Interim :> INTERIM = struct
  fun readUntilBlank () =
    case (TextIO.inputLine TextIO.stdIn) of
        (SOME s) => if s = "\n" then
                        ""
                    else
                        (s ^ (readUntilBlank ()))
      | NONE => OS.Process.terminate OS.Process.success

  fun repl () =
    let
    in
        print "> ";
        let val s = readUntilBlank ()
        in
            let val sexp = Parser.parseString s
            in
                case sexp of
                    (SOME n) => print "Successful parse\n"
                  | _ => print "Bad parse\n"
            end;
            repl ()
        end
    end
end
