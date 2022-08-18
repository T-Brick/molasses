structure GenDriver :> GENERATOR =
struct
  exception Unsupported of string

  type output =
    { all : GenFile.t list
    , cm : CMFile.t option
    , toplevel : FileName.t * Import.t list
    }

  structure CMGen  = MkGenerator(CMGenerator)
  structure SeqGen = MkGenerator(SeqGenerator)

  fun generate pathmap file =
    case #get Control.mode () of
      Control.Sequential => SeqGen.generate pathmap file
    | Control.Full => CMGen.generate pathmap file
    | Control.Dynamic => (
      (SeqGen.generate pathmap file)
      handle SeqGen.Unsupported _ => (
        Control.print "Couldn't load sequentially... trying full\n";
        CMGen.generate pathmap file
      )
    )

end
