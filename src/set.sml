structure Set :> SET = struct
  type ''a set = ''a list

  val empty = []

  fun add set elem =
    if Util.member elem set then
        set
    else
        elem :: set

  fun size set = List.length set

  fun fromList (x::xs) = add (fromList xs) x
    | fromList nil = empty
end
