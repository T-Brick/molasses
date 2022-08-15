signature ANNOTATIONS =
sig
  type anno
  type t = anno

  exception UnsupportedAnnotation

  datatype follow  = Default | Ignore
  datatype warnerr = Warn | Error | Ignore
  datatype scope   = StrDec | Dec | TopDec | Program

  type 'kind updateAnno = anno -> 'kind -> anno

  val allowFFI : bool updateAnno
  val allowSuccessorML : bool updateAnno
  structure SuccessorML : sig
    val allowDoDecs : bool updateAnno

    val allowExtendedConsts : bool updateAnno
    structure ExtendConst : sig
      val allowExtendedNumConsts  : bool updateAnno
      val allowExtendedTextConsts : bool updateAnno
    end

    val allowLineComments  : bool updateAnno
    val allowOptBar        : bool updateAnno
    val allowOptSemicolon  : bool updateAnno
    val allowOrPats        : bool updateAnno
    val allowRecordPunExps : bool updateAnno
    val allowSigWithtype   : bool updateAnno

    val allowVectorExpsAndPats : bool updateAnno
    structure Vectors : sig
      val allowVectorExps : bool updateAnno
      val allowVectorPats : bool updateAnno
    end
  end

  val forcedUsed : unit updateAnno

  val nonexhaustiveBind     : warnerr updateAnno
  val nonexhaustiveExnBind  : follow updateAnno
  val nonexhaustiveMatch    : warnerr updateAnno
  val nonexhaustiveExnMatch : follow updateAnno
  val nonexhaustiveRaise    : warnerr updateAnno
  val nonexhaustiveExnRaise : follow updateAnno

  val redundantBind  : warnerr updateAnno
  val redundantMatch : warnerr updateAnno
  val redundantRaise : warnerr updateAnno

  val resolveScope    : scope updateAnno
  val sequenceNonUnit : warnerr updateAnno
  val valrecConstr    : warnerr updateAnno
  val warnUnused      : bool updateAnno
end
