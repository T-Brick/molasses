
val help =
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

val _ = List.map print mlbPathVars

val pathmap = MLtonPathMap.getPathMap ()
val pathmap =
  List.concat (List.map MLtonPathMap.fromString mlbPathVars) @ pathmap

fun run () =
  case (files, outputs) of
    ([], _) => print help
  | (_, []) => List.app (Molasses.make pathmap) files
  | (_, _) =>
      ListPair.appEq
        (fn (file, out) => Molasses.makeTo pathmap file out)
        (files, outputs)
      handle ListPair.UnequalLengths => (
        print "Each file must have their own output";
        OS.Process.exit OS.Process.failure
      )

fun repl () =
  let
    fun mkSource dir = dir ^ "/molasses-sources1.cm"
    val sources =
      case outputs of
        [] => List.map (mkSource o Molasses.defaultDirectory) files
      | _  => List.map mkSource outputs
    val cmd = repl_cmd ^ " " ^ String.concatWith " " sources
  in
    OS.Process.exit (OS.Process.system cmd)
  end

val _ = if doHelp then print help else run ()
val _ = if doREPL then repl () else ()
