structure CMGenerator =
struct
  open MLBAst

  structure WFile = WrappedFile

  exception Unsupported of string

  type out =
    { cms : CMFile.t list
    , smls : WFile.t list
    , future : WFile.future
    , filter : StrExport.t list
    , gened : Generated.t
    }
  val emptyOut =
    { cms = []
    , smls = []
    , future = WFile.initFuture
    , filter = []
    , gened = Generated.empty
    }

  (* most recent at the front *)
  fun joinOut (out1 : out) (out2 : out) : out =
    { cms = (#cms out2) @ (#cms out1)
    , smls = (#smls out2) @ (#smls out1)
    , future = #future out2
    , filter = (#filter out2) @ (#filter out1)
    , gened = Generated.join ((#gened out2), (#gened out1))
    }

  fun mkFilter future kind vals =
    { cms = []
    , smls = []
    , future = future
    , filter =
        List.map (fn (t, t_opt) =>
          kind (MLBToken.toString t, Option.map MLBToken.toString t_opt)
        ) vals
    , gened = Generated.empty
    }

  fun createMark (source, {cms, smls, future, filter, gened}) =
    { cms = List.map (fn x => CMFile.createMark (source, x)) cms
    , smls = List.map (fn x => WFile.createMark (source, x)) smls
    , future = future
    , filter = List.map (fn x => StrExport.createMark (source, x)) filter
    , gened = Generated.createMark (source, gened)
    }

  fun create (fs as (dir, pathmap)) cfile acc basdec =
    case basdec of
      DecEmpty => emptyOut
    | DecMultiple {elems, ...} =>
        mkNewCM fs cfile NONE acc (FileName.newCM ())
          false ((Seq.toList o Seq.map (fn x => (x, NONE))) elems)
    | DecPathMLB {path, token} =>
        let
          val file = (FilePath.normalize o FilePath.join) (dir, path)
          val {result, used} = MLtonPathMap.expandPath pathmap file
          val unixpath = FilePath.toUnixPath path
          val () = print ("Loading file " ^ unixpath ^ "\n")
          val future = #future acc
        in
        (* a src file is only loaded once, so we can just output it directly *)
          case Generated.find (#gened acc) result of
            SOME (Generated.GenFile.SML g) =>
              { cms = [], smls = [g], future = future
              , filter = [], gened = Generated.empty }
          | SOME (Generated.GenFile.CM g) =>
              { cms = [g], smls = [], future = future
              , filter = [], gened = Generated.empty }
          | SOME (Generated.GenFile.Rename _) => raise Fail "Impossible State"
          | NONE => (
              if List.exists (fn s => s = "SML_LIB") used then
                let
                  val wfile = WFile.makeLib unixpath future
                  val wfile' =
                    WFile.createMark (MLBToken.getSource token, wfile)
                in
                  { cms = [], smls = [wfile'], future=future
                  , filter = [], gened = Generated.empty }
                end
              else
                let
                  val dir = FilePath.dirname result
                  val source = Source.loadFromFile result
                  val Ast basdec = MLBParser.parse source
                  val res = create (dir, pathmap) (SOME path) acc basdec
                in
                  createMark
                    ( MLBToken.getSource token
                    , res
                    )
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
          case Generated.find (#gened acc) result of
            SOME (Generated.GenFile.SML g) =>
              { cms = [], smls = [g], future = future
              , filter = [], gened = Generated.empty }
          | SOME (Generated.GenFile.CM g) =>
              { cms = [g], smls = [], future = future
              , filter = [], gened = Generated.empty }
          | SOME (Generated.GenFile.Rename _) => raise Fail "Impossible State"
          | NONE =>
            let
              val () = print "\tCouldn't find cached version, generating...\n"
              val wfile = WFile.make future result
              val wfile' = WFile.createMark (MLBToken.getSource token, wfile)
            in
              { cms = []
              , smls = [wfile']
              , future = WFile.futureImports wfile'
              , filter = []
              , gened = Generated.singleton
                          (SOME path, Generated.GenFile.SML wfile')
              }
            end
        end
    | DecBasis _ => raise Unsupported "MLB basis dec not supported"
    | DecLocalInEnd { locall, basdec1, inn, basdec2, endd } =>
        createMark
          ( MLBToken.getSource locall
          , mkNewCM fs NONE ((SOME o MLBToken.getSource) endd) acc
              (FileName.newCM ()) true
              [ (basdec1, (SOME o MLBToken.getSource) locall)
              , (basdec2, (SOME o MLBToken.getSource) inn)
              ]
          )
    | DecOpen _ => raise Unsupported "MLB open dec not supported"
    | DecStructure { structuree, elems, delims } =>
        mkFilter (#future acc)
          (fn e =>
            StrExport.Mark (MLBToken.getSource structuree, StrExport.Str e))
          ( List.map
              (fn v => (#strid v, Option.map #strid (#eqstrid v)))
              (Seq.toList elems)
          )
    | DecSignature { signaturee, elems, delims } =>
        mkFilter (#future acc)
          (fn e =>
            StrExport.Mark (MLBToken.getSource signaturee, StrExport.Sig e))
          ( List.map
              (fn v => (#sigid v, Option.map #sigid (#eqsigid v)))
              (Seq.toList elems)
          )
    | DecFunctor { functorr, elems, delims } =>
        mkFilter (#future acc)
          (fn e =>
            StrExport.Mark (MLBToken.getSource functorr, StrExport.Fun e))
          ( List.map
              (fn v => (#funid v, Option.map #funid (#eqfunid v)))
              (Seq.toList elems)
          )
    | DecAnn _ => raise Unsupported "MLB annotations not supported"
    | DecUnderscorePrim _ => raise Unsupported "MLB underscore not supported"
  and mkNewCM (fs as (dir, pathmap)) cfile tok_opt acc file allow_filter elems =
    let
      fun folder ((d, src_opt), out) =
        (joinOut out
          o (case src_opt of
                NONE => (fn x => x)
              | SOME src => (fn x => createMark (src, x)))
          o create fs NONE out) d
      val {cms, smls, future, filter, gened} = List.foldl folder acc elems
      val cm = CMFile.normalize
        ( CMFile.restrictExports
            (CMFile.library tok_opt file (smls, cms))
            filter
        )
      fun noFilter () =
        { cms = [cm]
        , smls = []
        , future = future
        , filter = filter
        , gened = Generated.insert gened (cfile, Generated.GenFile.CM cm)
        }
      fun appFilter () =
        let
          val rename_file = FileName.newSML ()
          val cm' = CMFile.addSource cm rename_file
          val rename = Generated.GenFile.Rename (rename_file, filter)
        in
          { cms = [cm']
          , smls = []
          , future = future
          , filter = []
          , gened = Generated.insert
                      (Generated.insert gened (NONE, rename))
                      (cfile, Generated.GenFile.CM cm')
          }
        end
    in
      case (allow_filter, filter) of
        (false, _) => noFilter ()
      | (_, [])    => noFilter ()
      | (true, _)  => appFilter ()
    end

  fun generate pathmap file =
    let
      val {result, used} = MLtonPathMap.expandPath pathmap file
      val unixpath = FilePath.toUnixPath file
      val () = print ("Loading file " ^ unixpath ^ "\n")

      val dir = FilePath.dirname result
      val source = Source.loadFromFile result
      val Ast basdec = MLBParser.parse source
      val res = create (dir, pathmap) (SOME file) emptyOut basdec
    in
      Generated.all (#gened res)
    end

end
