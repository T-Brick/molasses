structure Molasses : sig

  structure Control : CONTROL

  exception UnknownFile of string
  type outdir = string
  val defaultDirectory : string -> outdir

  type result =
    { cm : GenFile.t option
    , top : GenFile.t list
    }

  val makeTo : MLtonPathMap.t -> string -> outdir -> result
  val make : MLtonPathMap.t -> string -> result

  val makeTo' : string -> outdir -> result
  val make' : string -> result

end = struct

  structure Control = Control

  exception UnknownFile of string
  type outdir = string

  fun defaultDirectory file =
    let
      val fp = FilePath.fromUnixPath file
      val outdir =
        FilePath.toHostPath (FilePath.dirname fp)
        ^ "/" ^ #get Control.default_dir ()
    in
      outdir
    end

  type result =
    { cm : GenFile.t option
    , top : GenFile.t list
    }

  local
    fun writeOut dir out =
      let
        val file = dir ^ "/" ^ (
            FileName.toString (GenFile.name out)
          )
        val outstream = TextIO.openOut file
      in
        TextIO.output (outstream, GenFile.toString out);
        TextIO.output (outstream, "\n");
        TextIO.closeOut outstream
      end

    fun maker pathmap file outdir =
      let
        val fp = FilePath.fromUnixPath file
        val { all, cm, toplevel } = GenDriver.generate pathmap fp
        val _ = OS.Process.system ("rm -rf " ^ outdir)
        val _ = OS.Process.system ("mkdir " ^ outdir)
        val write = fn (out, _) => writeOut outdir out
      in
        List.foldl write () all;
        case cm of
          NONE => (* no cm implies all top-level *)
            { cm = NONE
            , top = all
            }
        | SOME cm_f => (
          write (GenFile.TopLevel toplevel, ());
          { cm = SOME (GenFile.CM cm_f)
          , top = [ GenFile.TopLevel toplevel ]
          }
        )
      end

    fun checkFile file =
      case OS.Path.ext file of
        SOME "mlb" => file
      | _ => raise UnknownFile file
  in
    fun makeTo pathmap = maker pathmap o checkFile
    fun make pathmap file = makeTo pathmap file (defaultDirectory file)

    fun makeTo' file = makeTo (MLtonPathMap.getPathMap ()) file
    fun make' file = make (MLtonPathMap.getPathMap ()) file
  end

end
