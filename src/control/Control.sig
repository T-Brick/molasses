signature CONTROL =
sig
  type 'a controller = { get : unit -> 'a, set : 'a -> unit }

  val verbose : bool controller
end
