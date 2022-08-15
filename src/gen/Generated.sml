structure Generated :> sig

  structure GenFile : sig
    datatype generated =
        CM of CMFile.t
      | SML of WrappedFile.t
      | Rename of FileName.t * StrExport.t list
    type t = generated

    val name : generated -> FileName.t
    val toString : generated -> string

    include MARKER where type marker = generated
  end

  type gdict
  type t = gdict

  exception NotFound

  val empty : gdict
  val isEmpty : gdict -> bool
  val singleton : (FilePath.t option * GenFile.t) -> gdict
  val size : gdict -> int
  val sizeAll : gdict -> int
  val insert : gdict -> (FilePath.t option * GenFile.t) -> gdict

  val findPath : gdict -> FilePath.t -> GenFile.t option
  val findName : gdict -> FileName.t -> GenFile.t option
  val all : gdict -> GenFile.t list

  val join : gdict * gdict -> gdict

  include MARKER where type marker = gdict

end = struct

  structure GenFile =
  struct
    datatype generated =
        CM of CMFile.t
      | SML of WrappedFile.t
      | Rename of FileName.t * StrExport.t list
    type t = generated

    fun apply (cf,sf,rf) g =
      case g of
        CM cm => cf cm
      | SML wf => sf wf
      | Rename r => rf r
    fun map (cf,sf,rf) = apply (CM o cf, SML o sf, Rename o rf)

    val name = apply (CMFile.name, WrappedFile.name, fn (p,_) => p)
    val toString =
      apply ( CMFile.toString
            , WrappedFile.toString
            , fn (_, v) => String.concatWith "\n"
                             (List.map StrExport.toRenameString v)
            )

    type marker = generated
    fun mark (src, g) =
      map ( fn cm => CMFile.mark (src, cm)
          , fn wf => WrappedFile.mark (src, wf)
          , fn x => x
          ) g
    fun createMark (src, g) =
      map ( fn cm => CMFile.createMark (src, cm)
          , fn wf => WrappedFile.createMark (src, wf)
          , fn x => x
          ) g
    fun removeMark g =
      map (CMFile.removeMark, WrappedFile.removeMark, fn x => x) g
  end

  structure StringDict = Dict(
    type t = string
    val compare = String.compare
  )

  exception NotFound = StringDict.NotFound

  (* first is all real files, second is all generated files *)
  type gdict = GenFile.t StringDict.t * GenFile.t StringDict.t
  type t = gdict

  val empty = (StringDict.empty, StringDict.empty)
  fun isEmpty (_, l) = StringDict.isEmpty l
  fun size (d, _) = StringDict.size d
  fun sizeAll (_, l) = StringDict.size l
  fun singleton (fopt, g) =
    case fopt of
      NONE => ( StringDict.empty
              , StringDict.singleton ((FileName.toString o GenFile.name) g, g)
              )
    | SOME f => ( StringDict.singleton (FilePath.toUnixPath f, g)
                , StringDict.singleton ((FileName.toString o GenFile.name) g, g)
                )
  fun insert (d,l) (fopt, g) =
    case fopt of
      NONE => (d, StringDict.insert l ((FileName.toString o GenFile.name) g, g))
    | SOME f => ( StringDict.insert d (FilePath.toUnixPath f, g)
                , StringDict.insert l ((FileName.toString o GenFile.name) g, g))

  fun findPath (d, _) = StringDict.find d o FilePath.toUnixPath
  fun findName (_, l) = StringDict.find l o FileName.toString
  fun all (_, l) = StringDict.listItems l

  fun join ((d1, l1), (d2, l2)) =
    (StringDict.unionWith #1 (d1, d2), StringDict.unionWith #1 (l1, l2))

  type marker = gdict
  fun mark (src,(d,l)) =
    ( StringDict.map (fn x => GenFile.mark (src, x)) d
    , StringDict.map (fn x => GenFile.mark (src, x)) l )
  fun createMark (src, (d,l)) =
    ( StringDict.map (fn x => GenFile.createMark (src, x)) d
    , StringDict.map (fn x => GenFile.createMark (src, x)) l )
  fun removeMark (d,l) =
    ( StringDict.map GenFile.removeMark d
    , StringDict.map GenFile.removeMark l )

end
