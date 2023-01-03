(* src/gen/internal/SeqGenerator.sml : 1.1-69.1 *)
(* molasses-file101.sml *)
structure SeqGenerator :> INTERNAL_GENERATOR =
  struct

    structure WFile = WrappedFile

    exception Unsupported of string
    exception IllegalState

    type acc = {smls : WFile.t list, future : WFile.future, gened : Generated.t}
    val emptyAcc =
      {smls = [], future = WFile.initFuture, gened = Generated.empty}

    fun isAccEmpty ({smls, ...} : acc) = List.null smls

    fun createMark (source, {smls, future, gened}) =
      { smls = List.map (fn x => WFile.createMark (source, x)) smls
      , future = future
      , gened = Generated.createMark (source, gened)
      }

    fun getFuture ({future, ...} : acc) = future
    fun getGened ({gened, ...} : acc) = gened
    fun getAll ({smls, ...} : acc) = List.rev (List.map GenFile.SML smls)

    fun foundSMLCache acc g = acc
    fun foundCMCache acc g = raise IllegalState

    fun foundLibrary (acc : acc) wfile =
      {smls = wfile :: # smls acc, future = # future acc, gened = # gened acc}

    fun makeSML (dir, pathmap) (acc : acc) create result =
      let val wfile = WFile.make (# future acc) result in
        { smls = wfile :: # smls acc
        , future = WFile.futureImports wfile
        , gened =
            Generated.insert (# gened acc) (SOME result, GenFile.SML wfile)
        }
      end
    fun makeMLB (dir, pathmap) acc create result =
      let val (dir, MLBAst.Ast basdec) = GeneratorUtil.getAst result in
        create (dir, pathmap) (SOME result) acc basdec
      end
    fun makeFilter acc kind vals =
      raise Unsupported "MLB structure level decs not supported"
    fun makeList (dir, pathmap) cfile tok_opt create acc file elems =
      let
        fun folder ((d, src_opt), a) =
          ((case src_opt of
              NONE => (fn x => x)
            | SOME src => (fn x => createMark (src, x))) o create (dir, pathmap)
                                                             NONE
                                                             a)
            d
      in
        List.foldl folder acc elems
      end
    fun makeLocal (dir, pathmap) cfile create acc (locall, inn, endd) ( basdec1
                                                                      , basdec2
                                                                      ) =
      raise Unsupported "MLB local dec not supported"
  end

