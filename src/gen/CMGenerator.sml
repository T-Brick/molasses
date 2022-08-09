structure CMGenerator =
struct
  open MLBAst

  structure WFile = WrappedFile

  exception Unsupported of string

  datatype gened = CM of CMFile.t | SML of WFile.t

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

  fun create dir cfile acc basdec =
    case basdec of
      DecEmpty => emptyOut
    | DecMultiple {elems, ...} =>
        let
          val file = FileName.newCM ()
          fun folder (d, out) = (joinOut out o create dir NONE out) d
          val {cms, smls, future, filter, gened} =
            List.foldl folder acc (Seq.toList elems)
          val cm = CMFile.normalize
            (CMFile.restrictExports (CMFile.library file (smls, cms)) filter)
        in
          { cms = [cm]
          , smls = []
          , future = future
          , filter = []
          , gened = (cfile, CM cm)::gened
          }
        end
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
    | DecLocalInEnd _ => raise Unsupported "MLB local dec not supported"
    | DecOpen _ => raise Unsupported "MLB open dec not supported"
    | DecStructure _ => raise Unsupported "MLB structure dec not supported"
    | DecSignature _ => raise Unsupported "MLB signature dec not supported"
    | DecFunctor _ => raise Unsupported "MLB functor dec not supported"
    | DecAnn _ => raise Unsupported "MLB annotations not supported"
    | DecUnderscorePrim _ => raise Unsupported "MLB underscore not supported"

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
