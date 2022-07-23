structure FileName : sig
  type filename
  type t = filename

  val newSML : unit -> t
  val newCM  : unit -> t

  val toString : filename -> string
end = struct
  datatype filename = SML of int | CM of int
  type t = filename

  val sml_counter = ref 0
  val cm_counter  = ref 1

  val sml_name = "mollasses-file"
  val cm_name  = "mollasses-sources"

  fun newSML () = SML (!sml_counter before sml_counter := !sml_counter + 1)
  val newCM  () = CM  (!cm_counter  before cm_counter  := !cm_counter  + 1)

  val toString =
   fn SML n => sml_name ^ Int.toString n ^ ".sml"
    | CM  n => cm_name  ^ Int.toString n ^ ".cm"
end
