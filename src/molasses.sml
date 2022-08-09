
val infile = List.hd (CommandLineArgs.positional ())

fun writeOut dir name toString (wrapped, _) =
  let
    val file = dir ^ "/" ^ (
        FileName.toString (name wrapped)
      )
    val outstream = TextIO.openOut file
  in
    TextIO.output (outstream, toString wrapped);
    TextIO.output (outstream, "\n");
    TextIO.closeOut outstream
  end

fun applyGened (c,s,r) =
 fn CMGenerator.CM cm => c cm
  | CMGenerator.SML sml => s sml
  | CMGenerator.Rename rename => r rename

val _ =
  case OS.Path.ext infile of
    SOME "mlb" =>
      let
        val fp = FilePath.fromUnixPath infile
        val dir = FilePath.toHostPath (FilePath.dirname fp) ^ "/.molasses"
        val _ = OS.Process.system ("rm -rf " ^ dir)
        val _ = OS.Process.system ("mkdir " ^ dir)
        val gened = CMGenerator.generate fp

        val name = applyGened (CMFile.name, WrappedFile.name, fn (p,_) => p)
        val toString = applyGened (CMFile.toString, WrappedFile.toString,
          fn (_, v) => String.concatWith "\n" (List.map StrExport.toString v)
        )
        val write = writeOut dir name toString
      in
        List.foldl (write) () gened
      end
  | SOME _ =>
      let
        val wrapped =
          WrappedFile.make
            (InfixDict.initialTopLevel, [])
            (FilePath.fromUnixPath infile)
      in
        print (WrappedFile.toString wrapped)
      end
  | NONE => raise Fail ""
