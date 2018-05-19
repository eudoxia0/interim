local
  open Util
  open Compiler
in
  fun compile input output =
    let val c = compileFile emptyCompiler input
    in
        writeStringToFile output (compilerCode c)
    end
end

fun main () =
  case CommandLine.arguments() of
      [input, output] => compile input output
    | _ => raise Fail "Bad invocation"

val _ = main ()
