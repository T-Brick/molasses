(* src/gen/internal/InternalGenerator.sig : 1.1-51.1 *)
(* molasses-file99.sml *)
signature INTERNAL_GENERATOR =
  sig
    exception Unsupported of string
    exception IllegalState

    type acc

    val emptyAcc : acc
    val isAccEmpty : acc -> bool

    val createMark : Source.t * acc -> acc

    val getFuture : acc -> WrappedFile.future
    val getGened : acc -> Generated.t
    val getAll : acc -> GenFile.t list

    val foundSMLCache : acc -> WrappedFile.t -> acc
    val foundCMCache : acc -> CMFile.t -> acc
    val foundLibrary : acc -> WrappedFile.t -> acc

    val makeSML : FilePath.t * MLtonPathMap.t
                    -> acc
                    -> acc GeneratorUtil.create
                    -> FilePath.t
                    -> acc
    val makeMLB : FilePath.t * MLtonPathMap.t
                    -> acc
                    -> acc GeneratorUtil.create
                    -> FilePath.t
                    -> acc
    val makeFilter : acc
                       -> (string * string option -> StrExport.t)
                       -> (MLBToken.t * MLBToken.t option) list
                       -> acc
    val makeList : FilePath.t * MLtonPathMap.t
                     -> FilePath.t option
                     -> Source.t option
                     -> acc GeneratorUtil.create
                     -> acc
                     -> FileName.t
                     -> (MLBAst.basdec * Source.t option) list
                     -> acc
    val makeLocal : FilePath.t * MLtonPathMap.t
                      -> FilePath.t option
                      -> acc GeneratorUtil.create
                      -> acc
                      -> MLBToken.t * MLBToken.t * MLBToken.t
                      -> MLBAst.basdec * MLBAst.basdec
                      -> acc
  end

