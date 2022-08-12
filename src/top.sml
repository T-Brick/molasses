
val help =
"Usage: molasses [ARGS] FILE ... FILE\n" ^
"Optional arguments:\n\
\  [-mlb-path-var 'K V'] Defining a mlb path variable.\n\
\  [-output D]           Outputs the file to directory D, specify multiple\n\
\                        directories for multiple sources. If blank then uses\n\
\                        '.molasses' where the source is located.\n\
\  [--help]              Prints this message.\n"

val mlbPathVars = CommandLineArgs.parseStrings "mlb-path-var"
val output = CommandLineArgs.parseStrings "output"
val doHelp = CommandLineArgs.parseFlag "help"
val files = CommandLineArgs.positional ()

val _ = List.map print mlbPathVars

val pathmap = MLtonPathMap.getPathMap ()
val pathmap =
  List.concat (List.map MLtonPathMap.fromString mlbPathVars) @ pathmap

fun run () =
  case (files, output) of
    ([], _) => print help
  | (_, []) => List.app (Molasses.make pathmap) files
  | (_, _) =>
      ListPair.appEq
        (fn (file, out) => Molasses.makeTo pathmap file out)
        (files, output)
      handle ListPair.UnequalLengths => (
        print "Each file must have their own output";
        OS.Process.exit OS.Process.failure
      )

val _ = if doHelp then print help else run ()
