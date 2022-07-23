
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

val _ =
  case OS.Path.ext infile of
    SOME "mlb" =>
      let
        val fp = FilePath.fromUnixPath infile
        val dir = FilePath.toHostPath (FilePath.dirname fp) ^ "/.molasses"
        val _ = OS.Process.system ("rm -rf " ^ dir)
        val _ = OS.Process.system ("mkdir " ^ dir)
        val (files, cms) = CMGenerator.generate fp

        val writeSML = writeOut dir WrappedFile.name WrappedFile.toString
        val writeCM  = writeOut dir CMFile.name CMFile.toString
      in
        List.foldl (writeSML) () files;
        List.foldl (writeCM) () cms
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
