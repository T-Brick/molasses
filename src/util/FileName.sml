structure FileName : sig
  type filename
  type t = filename

  val newSML : unit -> t
  val newCM  : unit -> t
  val fromLibrary : string -> t

  val isSML : filename -> bool
  val isCM : filename -> bool
  val isLib : filename -> bool

  val toString : filename -> string
  val eq : filename * filename -> bool
end = struct
  datatype filename = SML of int | CM of int | Lib of string
  type t = filename

  val sml_counter = ref 0
  val cm_counter  = ref 1

  val sml_name = "molasses-file"
  val cm_name  = "molasses-sources"

  fun newSML () = SML (!sml_counter before sml_counter := !sml_counter + 1)
  fun newCM  () = CM  (!cm_counter  before cm_counter  := !cm_counter  + 1)
  val fromLibrary = Lib o LibraryMap.convert

  val isSML = fn SML _ => true | _ => false
  val isCM  = fn CM _  => true | _ => false
  val isLib = fn Lib _ => true | _ => false

  val toString =
   fn SML n => sml_name ^ Int.toString n ^ ".sml"
    | CM  n => cm_name  ^ Int.toString n ^ ".cm"
    | Lib s => s
  val eq =
   fn (SML n, SML m) => n = m
    | (CM n , CM m ) => n = m
    | (Lib s, Lib t) => s = t
    | _ => false
end
