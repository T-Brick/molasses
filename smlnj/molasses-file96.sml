(* src/gen/util/GeneratorUtil.sml : 1.1-22.1 *)
(* molasses-file96.sml *)
structure GeneratorUtil : GENERATOR_UTIL =
  struct
    type 'acc create =
      FilePath.t * MLtonPathMap.t
        -> FilePath.t option
        -> 'acc
        -> MLBAst.basdec
        -> 'acc

    fun getFile pathmap file =
      let
        val {result, used} = MLtonPathMap.expandPath pathmap file
        val unixpath = FilePath.toUnixPath file
        val () = Control.print ("Loading file " ^ unixpath ^ "\n")
      in
        (result, used, unixpath)
      end

    fun getAst file =
      (FilePath.dirname file, (MLBParser.parse o Source.loadFromFile) file)
  end

