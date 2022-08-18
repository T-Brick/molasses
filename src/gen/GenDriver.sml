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

  fun generate pathmap file = CMGen.generate pathmap file

end
