structure InternalStruct : sig
  type t
  val new : unit -> t
  val toString : t -> string
end = struct
  type t = int
  val counter = ref 0
  val name = "InternalMollassesStructure"

  val new = fn () => !counter before counter := !counter + 1
  val toString = fn n => name ^ Int.toString n
end
