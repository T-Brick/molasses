structure CMGenerator =
struct
  open MLBAst

  structure WFile = WrappedFile

  exception Unsupported

  (* todo take this in *)
  val pathmap = MLtonPathMap.getPathMap ()

  fun build dir (basdec, acc as (last, rest, all, libs, cms)) =
    case basdec of
      DecEmpty => acc
    | DecMultiple {elems, ...} =>
        List.foldl (build dir) acc (Seq.toList elems)
    | DecPathMLB {path, token} => generateFrom acc path
    | DecPathSML {path, token} =>
        let
          val fp = FilePath.join (dir, path)
        in
          ( WFile.make (WFile.futureImports last) fp
          , last::rest
          , last::all
          , libs
          , cms
          )
        end
    | DecBasis _ => raise Unsupported
    | DecLocalInEnd _ => raise Unsupported
    | DecOpen _ => raise Unsupported
    | DecStructure _ => raise Unsupported
    | DecSignature _ => raise Unsupported
    | DecFunctor _ => raise Unsupported
    | DecAnn _ => raise Unsupported
    | DecUnderscorePrim _ => raise Unsupported
  and generateFrom (start as (last, rest, all, libs, cms)) file =
    let
      val {result, used} = MLtonPathMap.expandPath pathmap file
      val fp = FilePath.toUnixPath file
    in
      if List.exists (fn s => s = "SML_LIB") used
      then (last, rest, all, FileName.fromLibrary fp :: libs, cms)
      else
        let
          val dir = FilePath.dirname file
          val source = Source.loadFromFile file
          val Ast basdec = MLBParser.parse source
        in
          build dir (basdec, start)
        end
    end

  fun generate file =
    let
      val cmtop = FileName.newCM ()
      val (last, rest, all, libs, cms) =
        generateFrom (WFile.blank, [], [], [], []) file
      val top = (List.tl o List.rev) (last :: rest)
      val cm = CMFile.addSources (CMFile.group cmtop top) libs
    in
      ((List.tl o List.rev) (last::all), cm :: cms)
    end

end
