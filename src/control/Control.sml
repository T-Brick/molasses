structure Control =
struct
  type 'a controller = { get : unit -> 'a, set : 'a -> unit }

  fun asgn r v = r := v

  val verbose_ref : bool ref = ref false
  val verbose = { get = fn () => !verbose_ref, set = asgn verbose_ref }
end
