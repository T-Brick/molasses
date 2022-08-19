structure Control :> CONTROL =
struct
  type 'a controller = { get : unit -> 'a, set : 'a -> unit }

  fun asgn r v = r := v

  val verbose_ref : bool ref = ref false
  val verbose = { get = fn () => !verbose_ref, set = asgn verbose_ref }

  val print = fn s => if #get verbose () then print s else ()

  val sml_name_ref : string ref = ref "molasses-file"
  val cm_name_ref  : string ref = ref "molasses-sources"
  val str_name_ref : string ref = ref "InternalMolassesStructure"
  val sml_name = { get = fn () => !sml_name_ref, set = asgn sml_name_ref }
  val cm_name  = { get = fn () => !cm_name_ref , set = asgn cm_name_ref  }
  val str_name = { get = fn () => !str_name_ref, set = asgn str_name_ref }

  val default_dir_ref : string ref = ref ".molasses"
  val default_dir = { get = fn () => !default_dir_ref
                    , set = asgn default_dir_ref }

  datatype mode = Sequential | Full | Dynamic
  val mode_ref : mode ref = ref Dynamic
  val mode = { get = fn () => !mode_ref, set = asgn mode_ref }

  val libmap_ref : LibraryMap.t ref = ref LibraryMap.default
  val libmap = { get = fn () => !libmap_ref, set = asgn libmap_ref }
end
