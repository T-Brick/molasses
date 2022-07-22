structure CMGenerator =
struct
  open MLBAst

  structure WFile = WrappedFile

  exception Unsupported

  fun build dir (basdec, acc as (last, rest)) =
    case basdec of
      DecEmpty => raise Unsupported
    | DecMultiple {elems, ...} =>
        List.foldl (build dir) acc (Seq.toList elems)
    | DecPathMLB {path, token} => raise Unsupported
    | DecPathSML {path, token} =>
        (WFile.make (WFile.futureImports last) (FilePath.join (dir, path)), last::rest)
    | DecBasis _ => raise Unsupported
    | DecLocalInEnd _ => raise Unsupported
    | DecOpen _ => raise Unsupported
    | DecStructure _ => raise Unsupported
    | DecSignature _ => raise Unsupported
    | DecFunctor _ => raise Unsupported
    | DecAnn _ => raise Unsupported
    | DecUnderscorePrim _ => raise Unsupported

  fun generate file =
    let
      val dir = FilePath.dirname file
      val source = Source.loadFromFile file
      val Ast basdec = MLBParser.parse source
    in
      build dir (basdec, (WFile.blank, []))
    end

end
