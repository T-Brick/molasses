signature CONTROL =
sig
  type 'a controller = { get : unit -> 'a, set : 'a -> unit }

  val print : string -> unit

  val verbose : bool controller

  val sml_name : string controller
  val cm_name  : string controller
  val str_name : string controller

  val use_source : bool controller
  val recover_src : bool controller

  val default_dir : string controller

  datatype mode = Sequential | Full | Dynamic
  val mode : mode controller

  val libmap : LibraryMap.t controller
end
