structure FileName : sig
  type filename
  type t = filename

  val newSML : unit -> filename
  val newCM  : unit -> filename
  val fromLibrary : string -> filename
  val fromOriginal : string -> filename

  (* outputs initial filename *)
  val resetSML : unit -> filename
  val resetCM  : unit -> filename

  val isSML : filename -> bool
  val isCM : filename -> bool
  val isLib : filename -> bool
  val isOriginal : filename -> bool

  val toString : filename -> string
  val toPath : string * string -> filename -> string
  val eq : filename * filename -> bool
end = struct
  datatype filename =
      SML of int
    | CM of int
    | Lib of string * bool (* path and does it contain anchor *)
    | Orig of string
  type t = filename

  val sml_counter = ref 0
  val cm_counter  = ref 1

  fun newSML () = SML (!sml_counter before sml_counter := !sml_counter + 1)
  fun newCM  () = CM  (!cm_counter  before cm_counter  := !cm_counter  + 1)
  fun fromLibrary s =
    let
      val realPath = LibraryMap.convert (#get Control.libmap ()) s
    in
      Lib (realPath, String.isSubstring "$" realPath)
    end
  val fromOriginal = Orig

  fun resetSML () = SML (sml_counter := 0; !sml_counter)
  fun resetCM () = CM (cm_counter := 1; !cm_counter)

  val isSML  = fn SML _  => true | _ => false
  val isCM   = fn CM _   => true | _ => false
  val isLib  = fn Lib _  => true | _ => false
  val isOrig = fn Orig _ => true | _ => false
  val isOriginal = isOrig

  val toString =
   fn SML n => (#get Control.sml_name ()) ^ Int.toString n ^ ".sml"
    | CM  n => (#get Control.cm_name  ()) ^ Int.toString n ^ ".cm"
    | Lib (s,_) => s
    | Orig s => s
  fun toPath (abs, rel) f =
    case f of
      SML _ => OS.Path.concat (abs, toString f)
    | CM _  => OS.Path.concat (abs, toString f)
    | Lib (_,a) => if a then toString f else OS.Path.concat (rel, toString f)
    | Orig _ => toString f
  val eq =
   fn (SML n, SML m)   => n = m
    | (CM n , CM m )   => n = m
    | (Lib x, Lib y)   => x = y
    | (Orig s, Orig t) => s = t
    | _ => false
end
