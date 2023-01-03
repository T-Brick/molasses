(* src/gen/Generator.fun : 1.1-103.1 *)
(* molasses-file102.sml *)
functor MkGenerator (Gen : INTERNAL_GENERATOR) :> GENERATOR =
  struct

    open MLBAst
    structure WFile = WrappedFile

    type output =
      { all : GenFile.t list
      , cm : CMFile.t option
      , toplevel : FileName.t * Import.t list
      }

    open Gen

    fun loadFile (dir, pathmap) token acc path load =
      let
        val (result, used) = GeneratorUtil.getFile pathmap dir path
        val isLibPathVar = LibraryMap.isLibraryPathVar (# get Control.libmap ())
        fun nonLib () =
          createMark
            (MLBToken.getSource token, load (dir, pathmap) acc create result)
      in
        (* a src file is only loaded once, so we can just output it directly *)
        case Generated.findPath (getGened acc) result of
          SOME (GenFile.SML g) => foundSMLCache acc g
        | SOME (GenFile.CM g) => foundCMCache acc g
        | SOME (GenFile.Rename _) => raise IllegalState
        | SOME (GenFile.TopLevel _) => raise IllegalState
        | NONE =>
            (if List.exists isLibPathVar used then
               case WFile.makeLib path (getFuture acc) of
                 SOME lib =>
                   foundLibrary acc
                     (WFile.createMark (MLBToken.getSource token, lib))
               | NONE => nonLib ()
             else
               nonLib ())
      end
    and create (dir, pathmap) cfile acc basdec =
      case basdec of
        DecEmpty => acc
      | DecMultiple {elems, ...} =>
          makeList (dir, pathmap) cfile NONE create acc (FileName.newCM ())
            ((Seq.toList o Seq.map (fn x => (x, NONE))) elems)
      | DecPathMLB {path, token} =>
          loadFile (dir, pathmap) token acc path makeMLB
      | DecPathSML {path, token} =>
          loadFile (dir, pathmap) token acc path makeSML
      | DecBasis _ => raise Unsupported "MLB basis dec not supported"
      | DecLocalInEnd {locall, basdec1, inn, basdec2, endd} =>
          makeLocal (dir, pathmap) cfile create acc (locall, inn, endd)
            (basdec1, basdec2)
      | DecOpen _ => raise Unsupported "MLB open dec not supported"
      | DecStructure {structuree, elems, delims} =>
          makeFilter acc
            (fn e =>
               StrExport.Mark (MLBToken.getSource structuree, StrExport.Str e))
            (List.map (fn v => (# strid v, Option.map # strid (# eqstrid v)))
               (Seq.toList elems))
      | DecSignature {signaturee, elems, delims} =>
          makeFilter acc
            (fn e =>
               StrExport.Mark (MLBToken.getSource signaturee, StrExport.Sig e))
            (List.map (fn v => (# sigid v, Option.map # sigid (# eqsigid v)))
               (Seq.toList elems))
      | DecFunctor {functorr, elems, delims} =>
          makeFilter acc
            (fn e =>
               StrExport.Mark (MLBToken.getSource functorr, StrExport.Fun e))
            (List.map (fn v => (# funid v, Option.map # funid (# eqfunid v)))
               (Seq.toList elems))
      | DecAnn _ => raise Unsupported "MLB annotations not supported"
      | DecUnderscorePrim _ => raise Unsupported "MLB underscore not supported"

    fun generate pathmap file =
      let
        val _ = FileName.resetSML ()
        val top_cm = FileName.resetCM ()

        val (result, _) =
          GeneratorUtil.getFile pathmap (FilePath.fromFields ["."]) file
        val (dir, Ast basdec) = GeneratorUtil.getAst result
        val res = create (dir, pathmap) (SOME file) emptyAcc basdec

        val convertCM =
          Option.mapPartial
            (GenFile.apply (SOME, fn _ => NONE, fn _ => NONE, fn _ => NONE))
      in
        { all = getAll res
        , cm = convertCM (Generated.findName (getGened res) top_cm)
        , toplevel = (FileName.newSML (), (List.rev o # 2 o getFuture) res)
        }
      end

  end

