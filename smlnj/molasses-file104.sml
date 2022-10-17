(* src/top.sml : 1.1-105.1 *)
(* molasses-file104.sml *)
fun top_level () =
  let
    val help_msg =
      "Usage: molasses [ARGS] FILE ... FILE\n" ^ "Optional arguments:\n\
                                                 \  [-mlb-path-var 'K V'] Defining a mlb path variable.\n\
                                                 \  [-libmap F]           Imports a libmap from F.\n\
                                                 \  [-output D]           Outputs the file to directory D, specify multiple\n\
                                                 \                        directories for multiple sources. If blank then uses\n\
                                                 \                        '.molasses' where the source is located.\n\
                                                 \  [-v]                  Verbose output.\n\
                                                 \  [--repl]              Loads the files into a REPL after generating them.\n\
                                                 \  [-repl-cmd C]         When loading the files into the REPL, use command C\n\
                                                 \                        instead of the default 'rlwrap sml'\n\
                                                 \                        (does nothing without --repl flag)\n\
                                                 \  [--help]              Prints this message.\n"

    val mlbPathVars = CommandLineArgs.parseStrings "mlb-path-var"
    val libmap_files = CommandLineArgs.parseStrings "libmap"
    val outputs = CommandLineArgs.parseStrings "output"
    val repl_cmd = CommandLineArgs.parseString "repl-cmd" "rlwrap sml"

    val doREPL = CommandLineArgs.parseFlag "repl"
    val doHelp = CommandLineArgs.parseFlag "help"
    val () = # set Control.verbose (CommandLineArgs.parseSingleFlag "v")
    val () = # set Control.recover_src doREPL
    val () = if not doREPL then # set Control.mode Control.Full else ()

    val files = CommandLineArgs.positional ()

    val pathmap = MLtonPathMap.getPathMap ()
    val pathmap =
      List.concat (List.map MLtonPathMap.fromString mlbPathVars) @ pathmap
    val () = List.app (Molasses.loadLibraryMap) libmap_files

    fun help () = (print help_msg; OS.Process.exit OS.Process.success)

    fun make_repl_cmd files = repl_cmd ^ " " ^ String.concatWith " " files

    fun boot_repl files =
      if (OS.Process.isSuccess o OS.Process.system o make_repl_cmd) files then
        OS.Process.exit OS.Process.success
      else
        OS.Process.exit OS.Process.failure

    fun run () =
      case (files, outputs) of
        ([], _) => if doREPL then boot_repl [] else help ()
      | (_, []) => List.map (Molasses.make pathmap) files
      | (_, _) =>
          ListPair.mapEq (fn (file, out) => Molasses.makeTo pathmap file out)
            (files, outputs)
            handle
                ListPair.UnequalLengths =>
                  ( print "Each file must have their own output"
                  ; OS.Process.exit OS.Process.failure
                  )

    fun repl results =
      let
        fun getDir outdir file =
          let
            (* TODO: move this to utility file (along with molasses.sml version) *)
            val abs_outdir =
              OS.Path.mkAbsolute {path = outdir, relativeTo = "/"}
            val relative_outdir =
              OS.Path.mkRelative
                {path = OS.Path.dir file, relativeTo = abs_outdir}
          in
            (outdir, relative_outdir)
          end
        fun mkSource dir src_file file_opt =
          case file_opt of
            NONE => ""
          | SOME file =>
              "'"
              ^ (FileName.toPath (getDir dir src_file) o GenFile.name) file
              ^ "'"
        fun mkSources ({cm, top}, src_file, out) =
          (mkSource out src_file cm)
          ^ " "
          ^ (String.concatWith " " (List.map (mkSource out src_file o SOME) top))
        val sources =
          case outputs of
            [] =>
              ListPair.map
                (fn (r, f) => mkSources (r, f, Molasses.defaultDirectory f))
                (results, files)
          | _ =>
              ListPair.map (fn ((r, f), out) => mkSources (r, f, out))
                (ListPair.map (fn x => x) (results, files), outputs)
      in
        boot_repl sources
      end

    val results =
      if doHelp then
        help ()
      else
        (run ()
           handle
               exn as Molasses.UnknownFile s =>
                 if doREPL then
                   ( Control.print "Failed to load... booting to REPL.\n"
                   ; boot_repl files
                   )
                 else
                   raise exn)
    val _ = if doREPL then repl results else ()
  in
    ()
  end
structure InternalMolassesStructure0 = struct
	val top_level = top_level
end
