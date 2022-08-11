val id = fn x => x

(* ohno the value restriction! also doesn't parse *)
val id' : int -> int = id o fn y => y
