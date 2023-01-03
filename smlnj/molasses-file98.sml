(* src/gen/Generator.sig : 1.1-13.1 *)
(* molasses-file98.sml *)
signature GENERATOR =
  sig
    exception Unsupported of string

    type output = { all : GenFile.t list
                  , cm : CMFile.t option
                  , toplevel : FileName.t * Import.t list
                  }

    val generate : MLtonPathMap.t -> FilePath.t -> output
  end

