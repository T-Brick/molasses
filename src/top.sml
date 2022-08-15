
val help_msg =
"Usage: molasses [ARGS] FILE ... FILE\n" ^
"Optional arguments:\n\
\  [-mlb-path-var 'K V'] Defining a mlb path variable.\n\
\  [-output D]           Outputs the file to directory D, specify multiple\n\
\                        directories for multiple sources. If blank then uses\n\
\                        '.molasses' where the source is located.\n\
\  [--repl]              Loads the files into a REPL after generating them.\n\
\  [--repl-cmd C]        When loading the files into the REPL, use command C\n\
\                        instead of the default 'rlwrap sml'\n\
\                        (does nothing without --repl flag)\n\
\  [--help]              Prints this message.\n"

val mlbPathVars = CommandLineArgs.parseStrings "mlb-path-var"
val outputs = CommandLineArgs.parseStrings "output"
val repl_cmd = CommandLineArgs.parseString "repl-cmd" "rlwrap sml"

val doREPL = CommandLineArgs.parseFlag "repl"
val doHelp = CommandLineArgs.parseFlag "help"

val files = CommandLineArgs.positional ()

val pathmap = MLtonPathMap.getPathMap ()
val pathmap =
  List.concat (List.map MLtonPathMap.fromString mlbPathVars) @ pathmap

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
          dir ^ "/" ^ (FileName.toString o Generated.GenFile.name) file
    fun mkSources ({cm, top}, out) =
      (mkSource out cm) ^ " " ^ (mkSource out (SOME top))
    val sources =
      case outputs of
        [] =>
          ListPair.map
            (fn (r,f) => mkSources (r, Molasses.defaultDirectory f))
            (results, files)
      | _  => ListPair.map mkSources (results, outputs)
    val cmd = repl_cmd ^ " " ^ String.concatWith " " sources
  in
    OS.Process.exit (OS.Process.system cmd)
  end

val results = if doHelp then help () else run ()
val _ = if doREPL then repl results else ()
