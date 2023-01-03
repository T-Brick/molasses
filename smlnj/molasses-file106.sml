(* src/top-nj.sml : 1.1-29.1 *)
(* molasses-file106.sml *)
local
	open InternalMolassesStructure0
in
structure MolassesNJ =
  struct
    fun main (name, args) = let val () = top_level () in OS.Process.success end

    fun export () = SMLofNJ.exportFn ("molasses", main)

    val use =
      fn file =>
        let
          val out = Molasses.defaultDirectory file
          val {cm, top} = Molasses.make' file
          val _ =
            (* load CM files *)
            case FileUtils.getSource out file cm of
              NONE => ()
            | SOME f => use f
          val _ =
            List.map
              (fn f => Option.map use (FileUtils.getSource out file (SOME f)))
              top
        in
          ()
        end
  end

end
