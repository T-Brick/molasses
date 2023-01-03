structure Molasses : sig

  val version : { id : int list, system : string }

  structure Control : CONTROL

  exception UnknownFile of string
  exception UnknownFileExtension of string
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

  val version : { id : int list, system : string } =
    { id = [0, 1, 2]
    , system = "Molasses"
    }

  structure Control = Control

  exception UnknownFile of string
  exception UnknownFileExtension of string
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

    fun writeOut (dir, rel) out =
      let
        val file = FileName.toPath (dir, rel) (GenFile.name out)
        val () = Control.print ("Writing " ^ file ^ "...\n")
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
        then Control.print ("Tried writing " ^ file ^ " which exists!\n")
        else write ()
      end

    fun maker pathmap file outdir =
      let
        val fp = FilePath.fromUnixPath file
        val { all, cm, toplevel } = GenDriver.generate pathmap fp

        (* TODO: replace with SML *)
        fun prepDirectory () =
          ( OS.Process.system ("rm -rf " ^ outdir)
          ; OS.FileSys.mkDir outdir
          )
        val abs_outdir = OS.Path.mkAbsolute {path=outdir, relativeTo="/"}
        val relative_outdir =
          OS.Path.mkRelative {path=OS.Path.dir file, relativeTo=abs_outdir}

        fun write (out, i) =
          case ( #get Control.use_source () andalso GenFile.isSML out
               , #get Control.mode ()
               ) of
            (true, Control.Sequential) => i
          | (true, Control.Dynamic)    => i
          | _ =>
            ( if i = 0 then prepDirectory () else ()
            ; writeOut (outdir, relative_outdir) out
            ; i + 1
            )

        val total = List.foldl write 0 all
        val () = Control.print
          ("\nWrote " ^ (Int.toString total) ^ " non-TopLevel files!\n\n")
      in
        case cm of
          NONE => (* no cm implies all top-level *)
            { cm = NONE
            , top = all
            }
        | SOME cm_f => (
            write (GenFile.TopLevel toplevel, total + 1);
            { cm = SOME (GenFile.CM cm_f)
            , top = [ GenFile.TopLevel toplevel ]
            }
        )
      end

    fun checkFile file =
      case OS.Path.ext file of
        SOME "mlb" => file
      | SOME "sml" =>
        ( if #get Control.recover_src () then
            ( ignore
            o SafeParser.parse
            o Source.loadFromFile
            o FilePath.fromUnixPath) file
          else ()
        ; raise UnknownFile file
        )
      | SOME _ => raise UnknownFileExtension file
      | _ => raise UnknownFile file
  in
    fun makeTo pathmap file outdir = (maker pathmap o checkFile) file outdir
      handle
        UnknownFile file =>
          if #get Control.recover_src () then
            ( Control.print "Failed to load... attempting to recover.\n"
            ; { cm = NONE
              , top = [GenFile.TopLevel(FileName.fromOriginal file, [])]
              }
            )
          else raise UnknownFile file
      | exn =>
        ( print (
            "Encountered exception ("
            ^ exnMessage exn
            ^ ") when loading file: "
            ^ file ^
            "\n"
          )
        ; raise exn
        )
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
