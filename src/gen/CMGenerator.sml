structure CMGenerator =
struct
  open MLBAst

  structure WFile = WrappedFile

  exception Unsupported

  (* todo take this in *)
  val pathmap = MLtonPathMap.getPathMap ()

  fun isLoaded path : (FilePath.t * 'a) list -> (FilePath.t * 'a) option =
    List.find (fn (f,_) => FilePath.sameFile (path, f))

  fun build dir (basdec, acc as ((last_path, last_file), rest, all, libs, cms)) =
    case basdec of
      DecEmpty => acc
    | DecMultiple {elems, ...} =>
        List.foldl (build dir) acc (Seq.toList elems)
    | DecPathMLB {path, token} => generateFrom acc path
    | DecPathSML {path, token} =>
        let
          val () = print ("Expanding file " ^ (FilePath.toUnixPath path) ^ "\n")
          val {result, used} =
            MLtonPathMap.expandPath
              pathmap
              ((FilePath.normalize o FilePath.join) (dir, path))
          val () = print ("Loading file " ^ (FilePath.toUnixPath result) ^ "\n")
        in
          case isLoaded result ((last_path, last_file)::all) of
            SOME (_,_) => (
              print "Already loaded. Skipping...";
              acc
            )
          | NONE =>
            ( (result, WFile.make (WFile.futureImports (last_file)) result)
            , (last_path, last_file)::rest
            , (last_path, last_file)::all
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
      val unixpath = FilePath.toUnixPath file
      val () = print ("Loading file " ^ unixpath ^ "\n")
    in
      if List.exists (fn s => s = "SML_LIB") used
      then (last, rest, all, FileName.fromLibrary unixpath :: libs, cms)
      else
        let
          val dir = FilePath.dirname result
          val source = Source.loadFromFile result
          val Ast basdec = MLBParser.parse source
        in
          build dir (basdec, start)
        end
    end

  fun generate file =
    let
      val cmtop = FileName.newCM ()
      val (last, rest, all, libs, cms) =
        generateFrom ((FilePath.fromFields [""], WFile.blank), [], [], [], []) file
      val top = (List.map #2 o List.tl o List.rev) (last :: rest)
      val cm = CMFile.addSources (CMFile.group cmtop top) libs
      val out_all = (List.map #2 o List.tl o List.rev) (last::all)
    in
      (out_all, List.map CMFile.normalize (cm :: cms))
    end

end
