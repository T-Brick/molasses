structure Control :> CONTROL =
struct
  type 'a controller = { get : unit -> 'a, set : 'a -> unit }

  fun asgn r v = r := v

  val verbose_ref : bool ref = ref false
  val verbose = { get = fn () => !verbose_ref, set = asgn verbose_ref }

  fun print s =
    if #get verbose () then print s else ()

  val sml_name_ref : string ref = ref "molasses-file"
  val cm_name_ref  : string ref = ref "molasses-sources"
  val str_name_ref : string ref = ref "InternalMolassesStructure"
  val sml_name = { get = fn () => !sml_name_ref, set = asgn sml_name_ref }
  val cm_name  = { get = fn () => !cm_name_ref , set = asgn cm_name_ref  }
  val str_name = { get = fn () => !str_name_ref, set = asgn str_name_ref }
end

(* override top-level definition *)
val print : string -> unit = Control.print
