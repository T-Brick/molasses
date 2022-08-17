signature CONTROL =
sig
  type 'a controller = { get : unit -> 'a, set : 'a -> unit }

  val print : string -> unit

  val verbose : bool controller

  val sml_name : string controller
  val cm_name  : string controller
  val str_name : string controller
end
