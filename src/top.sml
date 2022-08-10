
val infile = List.hd (CommandLineArgs.positional ())
val _ = Molasses.make infile
