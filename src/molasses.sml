
structure Molasses : sig
  exception UnknownFile of string
  type outdir = string

  val makeTo : MLtonPathMap.t -> string -> outdir -> unit
  val make : MLtonPathMap.t -> string -> unit

  val makeTo' : string -> outdir -> unit
  val make' : string -> unit
end = struct
  exception UnknownFile of string
  type outdir = string

  local
    fun applyGened (c,s,r) =
    fn Generated.CM cm => c cm
      | Generated.SML sml => s sml
      | Generated.Rename rename => r rename

    val name = applyGened (CMFile.name, WrappedFile.name, fn (p,_) => p)
    val toString = applyGened (CMFile.toString, WrappedFile.toString,
      fn (_, v) => String.concatWith "\n" (List.map StrExport.toString v)
    )

    fun writeOut dir out =
      let
        val file = dir ^ "/" ^ (
            FileName.toString (name out)
          )
        val outstream = TextIO.openOut file
      in
        TextIO.output (outstream, toString out);
        TextIO.output (outstream, "\n");
        TextIO.closeOut outstream
      end

    fun maker pathmap file outdir =
      let
        val fp = FilePath.fromUnixPath file
        val gened = CMGenerator.generate pathmap fp
        val _ = OS.Process.system ("rm -rf " ^ outdir)
        val _ = OS.Process.system ("mkdir " ^ outdir)
        val write = fn (out, _) => writeOut outdir out
      in
        List.foldl write () gened
      end

    fun checkFile file =
      case OS.Path.ext file of
        SOME "mlb" => file
      | _ => raise UnknownFile file
  in
    fun makeTo pathmap = maker pathmap o checkFile
    fun make pathmap file =
      let
        val fp = FilePath.fromUnixPath file
        val outdir = FilePath.toHostPath (FilePath.dirname fp) ^ "/.molasses"
      in
        makeTo pathmap file outdir
      end

    fun makeTo' file = makeTo (MLtonPathMap.getPathMap ()) file
    fun make' file = make (MLtonPathMap.getPathMap ()) file
  end
end
