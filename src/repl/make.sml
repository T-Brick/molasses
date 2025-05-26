(* Generates the REPL heap image *)
val _ = SMLofNJ.exportML "molasses-repl"
val _ =
  let
    val {releaseDate:string, suffix:string, system:string, version_id:int list} = Compiler.version
    fun idstr id = "v" ^ String.concatWith "." (List.map Int.toString id)
    val smlnj = system ^ " " ^ idstr version_id ^ " [built: " ^ releaseDate ^ "]"

    val { id : int list, system : string } = Molasses.version
    val molasses = system ^ " " ^ idstr id
  in
    print smlnj;
    print " with ";
    print molasses;
    print "\n"
  end
