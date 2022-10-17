(* src/util/Marker.sig : 1.1-9.1 *)
(* molasses-file84.sml *)
signature MARKER =
  sig
    type marker

    val mark : Source.t * marker -> marker
    val createMark : Source.t * marker -> marker
    val removeMark : marker -> marker
  end

