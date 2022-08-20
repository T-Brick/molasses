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

  val loadLibraryMap : string -> unit

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
    fun exists dir f =
      let
        val dirstream = OS.FileSys.openDir dir
        fun findFile () =
          case OS.FileSys.readDir dirstream of
            NONE => (OS.FileSys.closeDir dirstream; false)
          | SOME f' => f = f' orelse findFile ()
      in
        findFile ()
      end

    fun writeOut dir out =
      let
        val filename = FileName.toString (GenFile.name out)
        val () = Control.print ("Writing " ^ filename ^ "...\n")
        val file = dir ^ "/" ^ filename
        fun write () =
          let
            val outstream = TextIO.openOut file
          in
            TextIO.output (outstream, GenFile.toString out);
            TextIO.output (outstream, "\n");
            TextIO.closeOut outstream
          end
      in
        if exists (OS.Path.dir file) (OS.Path.file file)
        then Control.print ("Tried writing " ^ filename ^ " which exists!\n")
        else write ()
      end

    fun maker pathmap file outdir =
      let
        val fp = FilePath.fromUnixPath file
        val { all, cm, toplevel } = GenDriver.generate pathmap fp
        (* TODO: replace with SML *)
        val _ = OS.Process.system ("rm -rf " ^ outdir)
        val _ = OS.FileSys.mkDir outdir
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

  fun loadLibraryMap path =
    let
      val file = FilePath.fromUnixPath path
      val instream = TextIO.openIn (FilePath.toUnixPath file)
      val text = TextIO.inputAll instream
      val new_libmap = LibraryMap.addFromString (#get Control.libmap ()) text
    in
      #set Control.libmap new_libmap
    end

end
