(* TODO: implement this! *)
structure Annotations : ANNOTATIONS =
struct
  type anno = unit
  type t = anno

  exception UnsupportedAnnotation of string

  datatype follow  = Default | Ignore
  datatype warnerr = Warn | Error | Ignore
  datatype scope   = StrDec | Dec | TopDec | Program

  type 'kind updateAnno = anno -> 'kind -> anno

  val allowFFI : bool updateAnno = raise UnsupportedAnnotation
  val allowSuccessorML : bool updateAnno = raise UnsupportedAnnotation
  structure SuccessorML =
  struct
    val allowDoDecs : bool updateAnno = raise UnsupportedAnnotation

    val allowExtendedConsts : bool updateAnno = raise UnsupportedAnnotation
    structure ExtendConst =
    struct
      val allowExtendedNumConsts  : bool updateAnno = raise UnsupportedAnnotation
      val allowExtendedTextConsts : bool updateAnno = raise UnsupportedAnnotation
    end

    val allowLineComments  : bool updateAnno = raise UnsupportedAnnotation
    val allowOptBar        : bool updateAnno = raise UnsupportedAnnotation
    val allowOptSemicolon  : bool updateAnno = raise UnsupportedAnnotation
    val allowOrPats        : bool updateAnno = raise UnsupportedAnnotation
    val allowRecordPunExps : bool updateAnno = raise UnsupportedAnnotation
    val allowSigWithtype   : bool updateAnno = raise UnsupportedAnnotation

    val allowVectorExpsAndPats : bool updateAnno = raise UnsupportedAnnotation
    structure Vectors =
    struct
      val allowVectorExps : bool updateAnno = raise UnsupportedAnnotation
      val allowVectorPats : bool updateAnno = raise UnsupportedAnnotation
    end
  end

  val forcedUsed : unit updateAnno = raise UnsupportedAnnotation

  val nonexhaustiveBind     : warnerr updateAnno = raise UnsupportedAnnotation
  val nonexhaustiveExnBind  : follow updateAnno = raise UnsupportedAnnotation
  val nonexhaustiveMatch    : warnerr updateAnno = raise UnsupportedAnnotation
  val nonexhaustiveExnMatch : follow updateAnno = raise UnsupportedAnnotation
  val nonexhaustiveRaise    : warnerr updateAnno = raise UnsupportedAnnotation
  val nonexhaustiveExnRaise : follow updateAnno = raise UnsupportedAnnotation

  val redundantBind  : warnerr updateAnno = raise UnsupportedAnnotation
  val redundantMatch : warnerr updateAnno = raise UnsupportedAnnotation
  val redundantRaise : warnerr updateAnno = raise UnsupportedAnnotation

  val resolveScope    : scope updateAnno = raise UnsupportedAnnotation
  val sequenceNonUnit : warnerr updateAnno = raise UnsupportedAnnotation
  val valrecConstr    : warnerr updateAnno = raise UnsupportedAnnotation
  val warnUnused      : bool updateAnno = raise UnsupportedAnnotation
end
