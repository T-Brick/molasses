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
    , gened : Generated.gdict
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

  (* todo take this in *)
  val pathmap = MLtonPathMap.getPathMap ()

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

  fun create dir cfile acc basdec =
    case basdec of
      DecEmpty => emptyOut
    | DecMultiple {elems, ...} =>
        mkNewCM dir cfile acc (FileName.newCM ()) false (Seq.toList elems)
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
            SOME (Generated.SML g) =>
              { cms = [], smls = [g], future = future
              , filter = [], gened = Generated.empty }
          | SOME (Generated.CM g) =>
              { cms = [g], smls = [], future = future
              , filter = [], gened = Generated.empty }
          | SOME (Generated.Rename _) => raise Fail "Impossible State"
          | NONE => (
              if List.exists (fn s => s = "SML_LIB") used then
                let
                  val wfile = WFile.makeLib unixpath future
                in
                  { cms = [], smls = [wfile], future=future
                  , filter = [], gened = Generated.empty }
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
          case Generated.find (#gened acc) result of
            SOME (Generated.SML g) =>
              { cms = [], smls = [g], future = future
              , filter = [], gened = Generated.empty }
          | SOME (Generated.CM g) =>
              { cms = [g], smls = [], future = future
              , filter = [], gened = Generated.empty }
          | SOME (Generated.Rename _) => raise Fail "Impossible State"
          | NONE =>
            let
              val () = print "\tCouldn't find cached version, generating...\n"
              val wfile = WFile.make future result
            in
              { cms = []
              , smls = [wfile]
              , future = WFile.futureImports wfile
              , filter = []
              , gened = Generated.singleton (SOME path, Generated.SML wfile)
              }
            end
        end
    | DecBasis _ => raise Unsupported "MLB basis dec not supported"
    | DecLocalInEnd { basdec1, basdec2, ... } =>
        mkNewCM dir NONE acc (FileName.newCM ()) true [basdec1, basdec2]
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
  and mkNewCM dir cfile acc file allow_filter elems =
    let
      fun folder (d, out) = (joinOut out o create dir NONE out) d
      val {cms, smls, future, filter, gened} = List.foldl folder acc elems
      val cm = CMFile.normalize
        (CMFile.restrictExports (CMFile.library file (smls, cms)) filter)
      fun noFilter () =
        { cms = [cm]
        , smls = []
        , future = future
        , filter = filter
        , gened = Generated.insert gened (cfile, Generated.CM cm)
        }
      fun appFilter () =
        let
          val rename_file = FileName.newSML ()
          val cm' = CMFile.addSource cm rename_file
          val rename = Generated.Rename (rename_file, filter)
        in
          { cms = [cm']
          , smls = []
          , future = future
          , filter = []
          , gened = Generated.insert
                      (Generated.insert gened (NONE, rename))
                      (cfile, Generated.CM cm')
          }
        end
    in
      case (allow_filter, filter) of
        (false, _) => noFilter ()
      | (_, [])    => noFilter ()
      | (true, _)  => appFilter ()
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
      Generated.all (#gened res)
    end

end
