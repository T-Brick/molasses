structure WrappedFile : sig
  type future = InfixDict.t * Import.t list
  type wrapped
  type t = wrapped

  val initFuture : future
  val blank : wrapped
  val make : future -> FilePath.t -> wrapped
  val makeLib : string -> future -> wrapped

  val futureImports : wrapped -> future
  val name : wrapped -> FileName.t
  val exports : wrapped -> StrExport.t list

  val toString : wrapped -> string
end = struct

  type future = InfixDict.t * Import.t list
  type wrapped =
    { name    : FileName.t
    , imports : Import.t list
    , istr    : (InternalStruct.t * ExpExport.t list) option
    , ast     : Ast.t
    , exports : StrExport.t list
    , future  : future
    }
  type t = wrapped

  val initFuture : future = (InfixDict.initialTopLevel, [])

  local
    fun makeEmpty name future =
      { name    = name
      , imports = []
      , istr    = NONE
      , ast     = Ast.Ast (Seq.empty ())
      , exports = []
      , future  = future
      }
  in
    val blank : wrapped =
      makeEmpty (FileName.newSML ()) initFuture
    val makeLib : string -> future -> wrapped =
      makeEmpty o FileName.fromLibrary
  end

  fun handleLexOrParseError exn =
  let
    val e =
      case exn of
        Error.Error e => e
      | other => raise other
    val hist = MLton.Exn.history exn
  in
    TerminalColorString.print
      (Error.show {highlighter = SOME SyntaxHighlighter.fuzzyHighlight} e);
    if List.null hist then () else
      print ("\n" ^ String.concat (List.map (fn ln => ln ^ "\n") hist));
    OS.Process.exit OS.Process.failure
  end

  fun make (fixities, imports) file =
    let
      val source = Source.loadFromFile file
      val (fixities, ast) = Parser.parseWithInfdict fixities source
        handle exn => handleLexOrParseError exn
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

  val futureImports : wrapped -> InfixDict.t * Import.t list = #future
  val name : wrapped -> FileName.t = #name
  val exports : wrapped -> StrExport.t list = #exports

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
