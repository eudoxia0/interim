structure Set :> SET = struct
  type ''a set = ''a list

  val empty = []

  fun member x nil = false
    | member x (y::ys) = (x = y) orelse member x ys

  fun add set elem =
    if member elem set then
        set
    else
        elem :: set

  fun size set = List.length set

  fun fromList (x::xs) = add x (fromList xs)
    | fromList nil = empty
end
