signature MARKER =
sig
  type marker

  val mark : Source.t * marker -> marker
  val createMark : Source.t * marker -> marker
  val removeMark : marker -> marker
end
