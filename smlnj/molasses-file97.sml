(* src/gen/util/GeneratorUtil.sml : 1.1-28.1 *)
(* molasses-file97.sml *)
structure GeneratorUtil : GENERATOR_UTIL =
  struct
    type 'acc create =
      FilePath.t * MLtonPathMap.t
        -> FilePath.t option
        -> 'acc
        -> MLBAst.basdec
        -> 'acc

    fun getFile pathmap relativeDir file =
      let
        val {result, used} = MLtonPathMap.expandPath pathmap file
        val unixpath = FilePath.toUnixPath file
        val () =
          Control.print
            ("Loading file "
             ^ unixpath
             ^ " as "
             ^ FilePath.toUnixPath result
             ^ "\n")
        val result =
          if FilePath.isAbsolute result then
            result
          else
            FilePath.normalize (FilePath.join (relativeDir, result))
      in
        (result, used)
      end

    fun getAst file =
      (FilePath.dirname file, (MLBParser.parse o Source.loadFromFile) file)
  end

