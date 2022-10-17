(* src/gen/util/GeneratorUtil.sig : 1.1-15.1 *)
(* molasses-file95.sml *)
signature GENERATOR_UTIL =
  sig
    type 'acc create = FilePath.t * MLtonPathMap.t
                         -> FilePath.t option
                         -> 'acc
                         -> MLBAst.basdec
                         -> 'acc

    val getFile : MLtonPathMap.t
                    -> FilePath.t
                    -> FilePath.t * string list * string
    val getAst : FilePath.t -> FilePath.t * MLBAst.t
  end

