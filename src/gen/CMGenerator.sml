structure CMGenerator =
struct
  open MLBAst

  structure WFile = WrappedFile

  exception Unsupported

  fun build dir (basdec, acc as (last, rest, all, cms)) =
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
  and generateFrom start file =
    let
      val dir = FilePath.dirname file
      val source = Source.loadFromFile file
      val Ast basdec = MLBParser.parse source
    in
      build dir (basdec, start)
    end

  fun generate file =
    let
      val cmtop = FileName.newCM ()
      val (last, rest, all, cms) = generateFrom (WFile.blank, [], [], []) file
      val top = (List.tl o List.rev) (last :: rest)
    in
      ((List.tl o List.rev) (last::all), (CMFile.group cmtop top) :: cms)
    end

end
