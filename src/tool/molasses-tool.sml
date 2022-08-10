
structure MolassesTool =
struct
  val _ =
    Tools.registerStdShellCmdTool
      { tool = "Molasses"
      , class = "molasses"
      , cmdStdPath = fn () => ("./molasses", [])
      , template = NONE
      , extensionStyle =
          Tools.RENAME (["mlb"], fn base => [(".molasses/molasses-sources1.cm", NONE, fn x=>x)])
      , dflopts = []
      }
end
