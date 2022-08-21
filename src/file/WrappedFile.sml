structure WrappedFile : sig
  type future = InfixDict.t * Import.t list
  type wrapped
  type t = wrapped

  val initFuture : future
  val blank : wrapped
  val make : future -> FilePath.t -> wrapped
  val makeLib : FilePath.t -> future -> wrapped

  val futureImports : wrapped -> future
  val exports : wrapped -> StrExport.t list

  include MARKER where type marker = wrapped
  val getSourceFile : wrapped -> FilePath.t option

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
    fun makeLib path future =
      let
        fun getFileName path = (
          FileName.fromLibrary (FilePath.toUnixPath path)
            handle LibraryMap.LibraryNotFound s =>
              case FilePath.toFields path of
                [] => raise Fail ""
              | _::rest => getFileName (FilePath.fromFields rest)
        )
        (* this has large big-O but unsure of a clean way to do better *)
        val filename = (
          getFileName path handle Fail _ =>
            raise LibraryMap.LibraryNotFound (FilePath.toUnixPath path)
        )
       in
        makeEmpty
          filename
          [ StrExport.Lib (FileName.toString filename) ]
          future
      end

  end

  fun make (fixities, imports) file =
    let
      val source = Source.loadFromFile file
      val (fixities, ast) = SafeParser.parseWithInfdict fixities source
      val (exp_exports, future, str_exports) = FindTopDecs.find ast
      val future = future @ imports
      val (future, istr, istr_export) =
        case (exp_exports, str_exports) of
            ([], []) => (future, SOME (InternalStruct.new (), []), [])
          | ([], _)  => (future, NONE, [])
          | _  =>
            let
              val istr_t = InternalStruct.new ()
              val istr_e =
                [StrExport.Str (InternalStruct.toString istr_t, NONE)]
            in
              (Import.Open istr_t :: future, SOME (istr_t, exp_exports), istr_e)
            end
    in
      Mark (source,
        Wrapped
          { name    = FileName.newSML ()
          , imports = imports
          , istr    = istr
          , ast     = ast
          , exports = istr_export @ str_exports
          , future  = (fixities, future)
          })
    end

  fun app f wf =
    case wf of
      Wrapped r => f r
    | Mark (_, wf') => app f wf'

  fun getSourceFile wfile =
    case wfile of
      Mark (src, _) => SOME (Source.fileName src)
    | Wrapped _ => NONE

  val futureImports : wrapped -> InfixDict.t * Import.t list = app #future
  fun name wfile =
    let
      val name = app #name wfile
      val orig = fn path => FileName.fromOriginal (FilePath.toUnixPath path)
    in
      if #get Control.use_source ()
        andalso not(FileName.isLib name)
      then (
        case (#get Control.mode (), getSourceFile wfile) of
          (Control.Sequential, SOME path) => orig path
        | (Control.Dynamic, SOME path)    => orig path
        | _ => name
      )
      else name
    end
  val exports : wrapped -> StrExport.t list = app #exports

  type marker = wrapped
  val mark = Mark
  fun createMark (src, wfile) =
    case wfile of
      Mark _ => wfile
    | Wrapped r => Mark (src, wfile)
  fun removeMark wfile =
    case wfile of
      Mark (_, wfile') => removeMark wfile'
    | Wrapped _ => wfile

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
