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

  fun member x nil = false
    | member x (y::ys) = (x = y) orelse member x ys

  fun position elem list =
    let fun index' nil _ = NONE
          | index' (head::tail) p = if head = elem then SOME p else index' tail (p+1)
    in
        index' list 0
    end
end
