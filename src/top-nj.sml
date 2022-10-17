
structure MolassesNJ =
struct
  val () = #set Control.mode Control.Full

  fun main (name, args) =
    let
      val () = top_level ()
    in
      OS.Process.success
    end

  fun export () =
    SMLofNJ.exportFn ("molasses", main)
end
