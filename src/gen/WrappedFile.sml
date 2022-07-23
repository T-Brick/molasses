structure WrappedFile : sig
  type wrapped
  type t = wrapped

  val blank : wrapped
  val make : InfixDict.t * Import.t list -> FilePath.t -> wrapped

  val futureImports : wrapped -> InfixDict.t * Import.t list
  val name : wrapped -> FileName.t

  val toString : wrapped -> string
end = struct

  type wrapped =
    { name    : FileName.t
    , imports : Import.t list
    , istr    : (InternalStruct.t * ExpExport.t list) option
    , ast     : Ast.t
    , exports : StrExport.t list
    , future  : InfixDict.t * Import.t list
    }
  type t = wrapped

  val blank : wrapped =
    { name    = FileName.newSML ()
    , imports = []
    , istr    = NONE
    , ast     = Ast.Ast (Seq.empty ())
    , exports = []
    , future  = (InfixDict.initialTopLevel, [])
    }
  fun make (fixities, imports) file =
    let
      val source = Source.loadFromFile file
      val (fixities, ast) = Parser.parseWithInfdict fixities source
      val (exp_exports, future, str_exports) = FindTopDecs.find ast
      val future = future @ imports
      val (future, istr) =
        case (exp_exports, str_exports) of
            ([], []) => (future, SOME (InternalStruct.new (), []))
          | ([], _)  => (future, NONE)
          | _  =>
            let
              val istr_t = InternalStruct.new ()
            in
              (Import.Open istr_t :: future, SOME (istr_t, exp_exports))
            end
    in
      { name    = FileName.newSML ()
      , imports = imports
      , istr    = istr
      , ast     = ast
      , exports = str_exports
      , future  = (fixities, future)
      }
    end

  fun futureImports (wrapped : t) = #future wrapped
  val name : t -> FileName.t = #name

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
    fun istrToString istr =
      case istr of
        NONE => ""
      | SOME (is, exps) =>
          "structure "
          ^ (InternalStruct.toString is)
          ^ " = struct\n\t"
          ^ (case exps of
              [] => "(* Empty structure to avoid CM errors *)"
            | _ => String.concatWith "\n\t" (List.map (ExpExport.toString) exps)
        ) ^ "\nend"
    fun importsToString imports body =
      case imports of
        [] => body
      | _  =>
          "local\n\t"
          ^ ( String.concatWith
                "\n\t"
                (List.map Import.toString (List.rev imports)) )
          ^ "\nin\n"
          ^ body
          ^ "\nend"
  in
    fun toString {name, imports, istr, ast, exports, future} =
      "(* " ^ (FileName.toString name) ^ " *)\n" ^
      importsToString imports (
        (astToString ast)
        ^ "\n"
        ^ istrToString istr
      )
  end

end
