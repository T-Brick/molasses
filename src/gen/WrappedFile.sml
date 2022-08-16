structure WrappedFile : sig
  type future = InfixDict.t * Import.t list
  type wrapped
  type t = wrapped

  val initFuture : future
  val blank : wrapped
  val make : future -> FilePath.t -> wrapped
  val makeLib : string -> future -> wrapped

  val futureImports : wrapped -> future
  val exports : wrapped -> StrExport.t list

  include MARKER where type marker = wrapped

  val name : wrapped -> FileName.t
  val toString : wrapped -> string
end = struct

  type future = InfixDict.t * Import.t list
  datatype wrapped =
      Wrapped of
        { name    : FileName.t
        , imports : Import.t list
        , istr    : (InternalStruct.t * ExpExport.t list) option
        , ast     : Ast.t
        , exports : StrExport.t list
        , future  : future
        }
    | Mark of Source.t * wrapped
  type t = wrapped

  val initFuture : future = (InfixDict.initialTopLevel, [])

  local
    fun makeEmpty name exports future =
      Wrapped
        { name    = name
        , imports = []
        , istr    = NONE
        , ast     = Ast.Ast (Seq.empty ())
        , exports = exports
        , future  = future
        }
  in
    val blank : wrapped = makeEmpty (FileName.newSML ()) [] initFuture
    fun makeLib name future =
      let
        val filename = FileName.fromLibrary name
       in
        makeEmpty
          filename
          [ StrExport.Lib (FileName.toString filename) ]
          future
      end

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
      Mark (source,
        Wrapped
          { name    = FileName.newSML ()
          , imports = imports
          , istr    = istr
          , ast     = ast
          , exports = str_exports
          , future  = (fixities, future)
          })
    end

  fun app f wf =
    case wf of
      Wrapped r => f r
    | Mark (_, wf') => app f wf'

  val futureImports : wrapped -> InfixDict.t * Import.t list = app #future
  val name : wrapped -> FileName.t = app #name
  val exports : wrapped -> StrExport.t list = app #exports

  type marker = wrapped
  val mark = Mark
  fun createMark (src, cmfile) =
    case cmfile of
      Mark _ => cmfile
    | Wrapped r => Mark (src, cmfile)
  fun removeMark cmfile =
    case cmfile of
      Mark (_, cmfile') => removeMark cmfile'
    | Wrapped _ => cmfile

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
    fun toString wf =
      case wf of
        Wrapped {name, imports, istr, ast, exports, future} =>
          "(* " ^ (FileName.toString name) ^ " *)\n" ^
          importsToString imports (
            (astToString ast)
            ^ "\n"
            ^ istrToString istr
          )
      | Mark (src, wf') =>
          "(* " ^ (Source.toRegionString src) ^ " *)\n" ^ (toString wf')
  end

end
