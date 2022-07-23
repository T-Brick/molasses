
val infile = List.hd (CommandLineArgs.positional ())

fun writeOut dir (wrapped, _) =
  let
    val file = dir ^ "/.mollasses/" ^ (
        FileName.toString (WrappedFile.name wrapped)
      )
    val outstream = TextIO.openOut file
  in
    TextIO.output (outstream, WrappedFile.toString wrapped);
    TextIO.output (outstream, "\n");
    TextIO.closeOut outstream
  end

val _ =
  case OS.Path.ext infile of
    SOME "mlb" =>
      let
        val fp = FilePath.fromUnixPath infile
        val dir = FilePath.toHostPath (FilePath.dirname fp)
        val (f,fs) = CMGenerator.generate fp
        val files = (List.tl o List.rev) (f::fs)
      in
        List.foldl (writeOut dir) () files
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
