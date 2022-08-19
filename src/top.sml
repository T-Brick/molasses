
val help_msg =
"Usage: molasses [ARGS] FILE ... FILE\n" ^
"Optional arguments:\n\
\  [-mlb-path-var 'K V'] Defining a mlb path variable.\n\
\  [-libmap F]           Imports a libmap from F.\n\
\  [-output D]           Outputs the file to directory D, specify multiple\n\
\                        directories for multiple sources. If blank then uses\n\
\                        '.molasses' where the source is located.\n\
\  [-v]                  Verbose output.\n\
\  [--repl]              Loads the files into a REPL after generating them.\n\
\  [--repl-cmd C]        When loading the files into the REPL, use command C\n\
\                        instead of the default 'rlwrap sml'\n\
\                        (does nothing without --repl flag)\n\
\  [--help]              Prints this message.\n"

val mlbPathVars = CommandLineArgs.parseStrings "mlb-path-var"
val libmap_files = CommandLineArgs.parseStrings "libmap"
val outputs = CommandLineArgs.parseStrings "output"
val repl_cmd = CommandLineArgs.parseString "repl-cmd" "rlwrap sml"

val doREPL = CommandLineArgs.parseFlag "repl"
val doHelp = CommandLineArgs.parseFlag "help"
val () = #set Control.verbose (CommandLineArgs.parseSingleFlag "v")

val files = CommandLineArgs.positional ()

val pathmap = MLtonPathMap.getPathMap ()
val pathmap =
  List.concat (List.map MLtonPathMap.fromString mlbPathVars) @ pathmap
val () = List.app (Molasses.loadLibraryMap) libmap_files

fun help () = (
  print help_msg;
  OS.Process.exit OS.Process.success
)

fun run () =
  case (files, outputs) of
    ([], _) => help ()
  | (_, []) => List.map (Molasses.make pathmap) files
  | (_, _) =>
      ListPair.mapEq
        (fn (file, out) => Molasses.makeTo pathmap file out)
        (files, outputs)
      handle ListPair.UnequalLengths => (
        print "Each file must have their own output";
        OS.Process.exit OS.Process.failure
      )

fun repl results =
  let
    fun mkSource dir file_opt =
      case file_opt of
        NONE => ""
      | SOME file =>
          dir ^ "/" ^ (FileName.toString o GenFile.name) file
    fun mkSources ({cm, top}, out) =
      (mkSource out cm) ^ " "
      ^ (String.concatWith " " (List.map (mkSource out o SOME) top))
    val sources =
      case outputs of
        [] =>
          ListPair.map
            (fn (r,f) => mkSources (r, Molasses.defaultDirectory f))
            (results, files)
      | _  => ListPair.map mkSources (results, outputs)
    val cmd = repl_cmd ^ " " ^ String.concatWith " " sources
  in
    if OS.Process.isSuccess (OS.Process.system cmd)
    then OS.Process.exit OS.Process.success
    else OS.Process.exit OS.Process.failure
  end

val results = if doHelp then help () else run ()
val _ = if doREPL then repl results else ()
