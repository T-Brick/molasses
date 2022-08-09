structure CMGenerator =
struct
  open MLBAst

  structure WFile = WrappedFile

  exception Unsupported of string

  datatype gened =
    CM of CMFile.t
  | SML of WFile.t
  | Rename of FileName.t * StrExport.t list

  type out =
    { cms : CMFile.t list
    , smls : WFile.t list
    , future : WFile.future
    , filter : StrExport.t list
    , gened : (FilePath.t option * gened) list
    }
  val emptyOut =
    { cms = []
    , smls = []
    , future = WFile.initFuture
    , filter = []
    , gened = []
    }

  (* most recent at the front *)
  fun joinOut (out1 : out) (out2 : out) : out =
    { cms = (#cms out2) @ (#cms out1)
    , smls = (#smls out2) @ (#smls out1)
    , future = #future out2
    , filter = (#filter out2) @ (#filter out1)
    , gened = (#gened out2) @ (#gened out1)
    }

  (* todo take this in *)
  val pathmap = MLtonPathMap.getPathMap ()

  fun isLoaded path : (FilePath.t option * 'a) list -> (FilePath.t option * 'a) option =
    List.find (fn (SOME f,_) => FilePath.sameFile (path, f) | _ => false)

  fun mkFilter future kind vals =
    (print "adding filter....\n\n";
    { cms = []
    , smls = []
    , future = future
    , filter =
        List.map (fn (t, t_opt) =>
          kind (MLBToken.toString t, Option.map MLBToken.toString t_opt)
        ) vals
    , gened = []
    })

  fun create dir cfile acc basdec =
    case basdec of
      DecEmpty => emptyOut
    | DecMultiple {elems, ...} =>
        mkNewCM dir cfile acc (FileName.newCM ()) (Seq.toList elems)
    | DecPathMLB {path, token} =>
        let
          val file = (FilePath.normalize o FilePath.join) (dir, path)
          val {result, used} = MLtonPathMap.expandPath pathmap file
          val unixpath = FilePath.toUnixPath path
          val () = print ("Loading file " ^ unixpath ^ "\n")
          val future = #future acc
        in
        (* a src file is only loaded once, so we can just output it directly *)
          case isLoaded result (#gened acc) of
            SOME (_,SML g) =>
              { cms = [], smls = [g], future = future, filter = [], gened = [] }
          | SOME (_, CM g) =>
              { cms = [g], smls = [], future = future, filter = [], gened = [] }
          | SOME (_, Rename _) => raise Fail "Impossible State"
          | NONE => (
              if List.exists (fn s => s = "SML_LIB") used then
                let
                  val wfile = WFile.makeLib unixpath future
                in
                  { cms = []
                  , smls = [wfile]
                  , future=future
                  , filter = []
                  , gened = []
                  }
                end
              else
                let
                  val dir = FilePath.dirname result
                  val source = Source.loadFromFile result
                  val Ast basdec = MLBParser.parse source
                in
                  create dir (SOME path) acc basdec
                end
          )
        end
    | DecPathSML {path, token} =>
        let
          val {result, used} =
            MLtonPathMap.expandPath
              pathmap
              ((FilePath.normalize o FilePath.join) (dir, path))
          val () = print ("Loading file " ^ (FilePath.toUnixPath result) ^ "\n")
          val future = #future acc
        in
        (* a src file is only loaded once, so we can just output it directly *)
          case isLoaded result (#gened acc) of
            SOME (_,SML g) =>
              { cms = [], smls = [g], future = future, filter = [], gened = [] }
          | SOME (_, CM g) =>
              { cms = [g], smls = [], future = future, filter = [], gened = [] }
          | SOME (_, Rename _) => raise Fail "Impossible State"
          | NONE =>
            let
              val wfile = WFile.make future result
            in
              { cms = []
              , smls = [wfile]
              , future = WFile.futureImports wfile
              , filter = []
              , gened = [(SOME path, SML wfile)]
              }
            end
        end
    | DecBasis _ => raise Unsupported "MLB basis dec not supported"
    | DecLocalInEnd { basdec1, basdec2, ... } =>
        let
          val file = FileName.newCM ()
          val _ = print (FileName.toString file)
          val _ = print "\n\n\n"
        in
        mkNewCM dir NONE acc (file) [basdec1, basdec2]
        end
    | DecOpen _ => raise Unsupported "MLB open dec not supported"
    | DecStructure { structuree, elems, delims } =>
        mkFilter (#future acc) StrExport.Str (
          List.map
            (fn v => (#strid v, Option.map #strid (#eqstrid v)))
            (Seq.toList elems)
        )
    | DecSignature { signaturee, elems, delims } =>
        mkFilter (#future acc) StrExport.Sig (
          List.map
            (fn v => (#sigid v, Option.map #sigid (#eqsigid v)))
            (Seq.toList elems)
        )
    | DecFunctor { functorr, elems, delims } =>
        mkFilter (#future acc) StrExport.Fun (
          List.map
            (fn v => (#funid v, Option.map #funid (#eqfunid v)))
            (Seq.toList elems)
        )
    | DecAnn _ => raise Unsupported "MLB annotations not supported"
    | DecUnderscorePrim _ => raise Unsupported "MLB underscore not supported"
  and mkNewCM dir cfile acc file elems =
    let
      fun folder (d, out) = (joinOut out o create dir NONE out) d
      val {cms, smls, future, filter, gened} = List.foldl folder acc elems
      val cm = CMFile.normalize
        (CMFile.restrictExports (CMFile.library file (smls, cms)) filter)
    in
      case [] of
        [] =>
          { cms = [cm]
          , smls = []
          , future = future
          , filter = []
          , gened = (cfile, CM cm)::gened
          }
      | _ =>
          let
            val () = print "renaming..."
            val rename_file = FileName.newSML ()
            val cm' = CMFile.addSource cm rename_file
            val rename = Rename (rename_file, filter)
          in
            { cms = [cm']
            , smls = []
            , future = future
            , filter = []
            , gened = (cfile, CM cm')::(NONE, rename)::gened
            }
        end
    end

  fun generate file =
    let
      val {result, used} = MLtonPathMap.expandPath pathmap file
      val unixpath = FilePath.toUnixPath file
      val () = print ("Loading file " ^ unixpath ^ "\n")

      val dir = FilePath.dirname result
      val source = Source.loadFromFile result
      val Ast basdec = MLBParser.parse source
      val res = create dir (SOME file) emptyOut basdec
    in
      List.map #2 (#gened res)
    end

end
