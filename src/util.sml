structure Util :> UTIL = struct
  type path = string

  fun readFileToString filepath =
    let val stream = TextIO.openIn filepath
        fun loop stream =
          case TextIO.inputLine stream of
              SOME line => line :: loop stream
            | NONE      => []
    in
        String.concat (loop stream before TextIO.closeIn stream)
    end

  fun writeStringToFile filepath str =
    let val stream = TextIO.openOut filepath
    in
        TextIO.output (stream, str) handle e => (TextIO.closeOut stream; raise e);
        TextIO.closeOut stream
    end
end
