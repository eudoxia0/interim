signature COMPILER = sig
    type compiler

    val emptyCompiler : compiler
    val compilerCode : compiler -> string

    val compileString : compiler -> string -> compiler
    val compileFile : compiler -> string -> compiler
end
