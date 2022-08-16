structure CMGenerator : sig
  exception Unsupported of string

  type output =
    { all : Generated.GenFile.t list
    , cm : CMFile.t option
    , toplevel : FileName.t * Import.t list
    }

  val generate : MLtonPathMap.t -> FilePath.t -> output
end = struct
  open MLBAst

  structure WFile = WrappedFile

  exception Unsupported of string
  exception IllegalState

  type output =
    { all : Generated.GenFile.t list
    , cm : CMFile.t option
    , toplevel : FileName.t * Import.t list
    }

  type acc =
    { cms : CMFile.t list
    , smls : WFile.t list
    , future : WFile.future
    , filter : StrExport.t list * bool (* export list, does rename *)
    , gened : Generated.t
    }
  val emptyAcc =
    { cms = []
    , smls = []
    , future = WFile.initFuture
    , filter = ([], false)
    , gened = Generated.empty
    }

  fun isAccEmpty ({cms, smls, ...} : acc) =
    case (cms, smls) of
      ([], []) => true
    | _ => false

  fun mkFilter acc kind vals =
    { cms = #cms acc
    , smls = #smls acc
    , future = #future acc (* TODO Change I think *)
    , filter =
        List.foldr (fn ((t, t_opt), (l,b)) =>
          ( kind (MLBToken.toString t, Option.map MLBToken.toString t_opt) :: l
          , b orelse Option.isSome t_opt )
        ) (#filter acc) vals
    , gened = #gened acc
    }

  fun createMark (source, {cms, smls, future, filter, gened}) =
    { cms = List.map (fn x => CMFile.createMark (source, x)) cms
    , smls = List.map (fn x => WFile.createMark (source, x)) smls
    , future = future
    , filter =
        ( List.map (fn x => StrExport.createMark (source, x)) (#1 filter)
        , #2 filter )
    , gened = Generated.createMark (source, gened)
    }

  fun create (fs as (dir, pathmap)) cfile acc basdec =
    case basdec of
      DecEmpty => acc
    | DecMultiple {elems, ...} =>
        mkNewCM fs cfile NONE acc (FileName.newCM ())
          ((Seq.toList o Seq.map (fn x => (x, NONE))) elems)
    | DecPathMLB {path, token} =>
        let
          val file = (FilePath.normalize o FilePath.join) (dir, path)
          val {result, used} = MLtonPathMap.expandPath pathmap file
          val unixpath = FilePath.toUnixPath path
          val () = print ("Loading file " ^ unixpath ^ "\n")
          val future = #future acc
        in
        (* a src file is only loaded once, so we can just output it directly *)
          case Generated.findPath (#gened acc) result of
            SOME (Generated.GenFile.SML g) =>
              { cms = #cms acc, smls = g :: #smls acc, future = future
              , filter = #filter acc, gened = #gened acc }
          | SOME (Generated.GenFile.CM g) =>
              { cms = g :: #cms acc, smls = #smls acc, future = future
              , filter = #filter acc, gened = #gened acc }
          | SOME (Generated.GenFile.Rename _) => raise IllegalState
          | SOME (Generated.GenFile.TopLevel _) => raise IllegalState
          | NONE => (
              if List.exists (fn s => s = "SML_LIB") used then
                let
                  val wfile = WFile.makeLib unixpath future
                  val wfile' =
                    WFile.createMark (MLBToken.getSource token, wfile)
                in
                  { cms = #cms acc, smls = wfile' :: #smls acc, future = future
                  , filter = #filter acc, gened = #gened acc }
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
          case Generated.findPath (#gened acc) result of
            SOME (Generated.GenFile.SML g) =>
              { cms = #cms acc, smls = g :: #smls acc, future = future
              , filter = #filter acc, gened = #gened acc }
          | SOME (Generated.GenFile.CM g) =>
              { cms = g :: #cms acc, smls = #smls acc, future = future
              , filter = #filter acc, gened = #gened acc }
          | SOME (Generated.GenFile.Rename _) => raise IllegalState
          | SOME (Generated.GenFile.TopLevel _) => raise IllegalState
          | NONE =>
            let
              val () = print "\tCouldn't find cached version, generating...\n"
              val wfile = WFile.make future result
              val wfile' = WFile.createMark (MLBToken.getSource token, wfile)
            in
              { cms = #cms acc
              , smls = wfile' :: #smls acc
              , future = WFile.futureImports wfile'
              , filter = #filter acc
              , gened = Generated.insert (#gened acc)
                          (SOME path, Generated.GenFile.SML wfile')
              }
            end
        end
    | DecBasis _ => raise Unsupported "MLB basis dec not supported"
    | DecLocalInEnd { locall, basdec1, inn, basdec2, endd } =>
        (* Idea - split local into 2 CM files: all the information prior to the
         * local dec and all the parts in the local declaration. Then wrap all
         * these into a new CM file, referencing the two CM files we just def'd
         *)
        let
          val new_cm = FileName.newCM ()
          val (old_cm, new_acc) =
            if isAccEmpty acc
            then (new_cm, acc)
            else
              let
                val old = FileName.newCM ()
              in
                (old, mkNewCM fs cfile NONE acc old [])
              end
          val {cms, smls, future, filter, gened} =
            createMark
              ( MLBToken.getSource endd
              , mkNewCM fs NONE ((SOME o MLBToken.getSource) endd)
                  new_acc (new_cm)
                  [ (basdec1, (SOME o MLBToken.getSource) locall)
                  , (basdec2, (SOME o MLBToken.getSource) inn) ]
              )
        in
          case Generated.findName gened old_cm of
            SOME (Generated.GenFile.CM g) =>
              { cms = g::cms, smls = smls, future = future
              , filter = ([], false), gened = gened }
          | _ => raise Fail "Couldn't find file that should be generated..."
        end
    | DecOpen _ => raise Unsupported "MLB open dec not supported"
    | DecStructure { structuree, elems, delims } =>
        mkFilter acc
          (fn e =>
            StrExport.Mark (MLBToken.getSource structuree, StrExport.Str e))
          ( List.map
              (fn v => (#strid v, Option.map #strid (#eqstrid v)))
              (Seq.toList elems)
          )
    | DecSignature { signaturee, elems, delims } =>
        mkFilter acc
          (fn e =>
            StrExport.Mark (MLBToken.getSource signaturee, StrExport.Sig e))
          ( List.map
              (fn v => (#sigid v, Option.map #sigid (#eqsigid v)))
              (Seq.toList elems)
          )
    | DecFunctor { functorr, elems, delims } =>
        mkFilter acc
          (fn e =>
            StrExport.Mark (MLBToken.getSource functorr, StrExport.Fun e))
          ( List.map
              (fn v => (#funid v, Option.map #funid (#eqfunid v)))
              (Seq.toList elems)
          )
    | DecAnn _ => raise Unsupported "MLB annotations not supported"
    | DecUnderscorePrim _ => raise Unsupported "MLB underscore not supported"
  and mkNewCM (fs as (dir, pathmap)) cfile tok_opt acc file elems =
    let
      fun folder ((d, src_opt), a) =
        ( (case src_opt of
              NONE => (fn x => x)
            | SOME src => (fn x => createMark (src, x))
          ) o create fs NONE a ) d
      val {cms, smls, future, filter, gened} = List.foldl folder acc elems
      val cm = CMFile.normalize
        ( CMFile.restrictExports
            (CMFile.library tok_opt file (smls, cms))
            (#1 filter)
        )
      val () = print ("Finished: " ^ FileName.toString file ^ "\n")
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
          val rename = Generated.GenFile.Rename (rename_file, #1 filter)
        in
          { cms = [cm']
          , smls = []
          , future = future
          , filter = ([], false)
          , gened = Generated.insert
                      (Generated.insert gened (NONE, rename))
                      (cfile, Generated.GenFile.CM cm')
          }
        end
    in
      case filter of
        (_, false) => noFilter ()
      | ([], _)    => noFilter ()
      | (_, true)  => appFilter ()
    end

  fun generate pathmap file =
    let
      val _ = FileName.resetSML ()
      val top_cm = FileName.resetCM ()
      val {result, used} = MLtonPathMap.expandPath pathmap file
      val unixpath = FilePath.toUnixPath file
      val () = print ("Loading file " ^ unixpath ^ "\n")

      val dir = FilePath.dirname result
      val source = Source.loadFromFile result
      val Ast basdec = MLBParser.parse source
      val res = create (dir, pathmap) (SOME file) emptyAcc basdec

      val convertCM = Option.mapPartial (
        Generated.GenFile.apply (SOME, fn _ => NONE, fn _ => NONE, fn _ => NONE)
      )
    in
      { all = Generated.all (#gened res)
      , cm = convertCM (Generated.findName (#gened res) top_cm)
      , toplevel = (FileName.newSML (), (#2 o #future) res)
      }
    end

end
