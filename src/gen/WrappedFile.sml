structure WrappedFile : sig
  type wrapped
  type t = wrapped

  val blank : wrapped
  val make : InfixDict.t * Import.t list -> FilePath.t -> wrapped
  val futureImports : wrapped -> InfixDict.t * Import.t list
  val toString : wrapped -> string
end = struct

  type wrapped =
    { imports : Import.t list
    , istr    : InternalStruct.t
    , ast     : Ast.t
    , exports : Export.t list
    , future  : InfixDict.t * Import.t list
    }
  type t = wrapped

  val blank : wrapped =
    { imports = []
    , istr    = InternalStruct.new ()
    , ast     = Ast.Ast (Seq.empty ())
    , exports = []
    , future  = (InfixDict.initialTopLevel, [])
    }
  fun make (fixities, imports) file =
    let
      val source = Source.loadFromFile file
      val (fixities, ast) = Parser.parseWithInfdict fixities source
      val (exports, future) = FindTopDecs.find ast
      val istr = InternalStruct.new ()
    in
      { imports = imports
      , istr    = istr
      , ast     = ast
      , exports = exports
      , future  = (fixities, Import.Open istr :: (future @ imports))
      }
    end

  fun futureImports (wrapped : t) = #future wrapped

  local
    fun astToString ast =
      TerminalColorString.toString {colors=false} (
        PrettyPrintAst.pretty
          { ribbonFrac=1.0
          , maxWidth=80
          , tabWidth=4
          , indent=2
          } ast
      )
  in
    fun toString {imports, istr, ast, exports, future} =
      let
        val istr_string = (InternalStruct.toString istr)
        val import_strings = List.map Import.toString (List.rev imports)
        val export_strings = List.map (Export.toString) exports
      in
        "local\n\t"
        ^ (String.concatWith "\n\t" import_strings)
        ^ "\nin\n"
        ^ (astToString ast)
        ^ "\n\nstructure "
        ^ istr_string
        ^ " = struct\n\t"
        ^ (String.concatWith "\n\t" export_strings)
        ^ "\nend (* structure "
        ^ istr_string
        ^ " *)\n\t"
        ^ "\nend"
      end
  end

end
