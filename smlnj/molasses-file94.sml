(* src/file/FileUtils.sml : 1.1-18.1 *)
(* molasses-file94.sml *)
structure FileUtils =
  struct
    fun getDir outdir file =
      let
        (* TODO: move this to utility file (along with molasses.sml version) *)
        val abs_outdir = OS.Path.mkAbsolute {path = outdir, relativeTo = "/"}
        val relative_outdir =
          OS.Path.mkRelative {path = OS.Path.dir file, relativeTo = abs_outdir}
      in
        (outdir, relative_outdir)
      end

    fun getSource dir src_file file_opt =
      Option.map
        (fn file => (FileName.toPath (getDir dir src_file) o GenFile.name) file)
        file_opt
  end

