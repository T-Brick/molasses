structure Generated :> sig

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

  structure StringDict = Dict(
    type t = string
    val compare = String.compare
  )

  exception NotFound = StringDict.NotFound

  (* first is all real files, second is all generated files *)
  type gdict = FileName.t StringDict.t * GenFile.t StringDict.t
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
    | SOME f => ( StringDict.singleton (FilePath.toUnixPath f, GenFile.name g)
                , StringDict.singleton ((FileName.toString o GenFile.name) g, g)
                )
  fun insert (d,l) (fopt, g) =
    case fopt of
      NONE => (d, StringDict.insert l ((FileName.toString o GenFile.name) g, g))
    | SOME f => ( StringDict.insert d (FilePath.toUnixPath f, GenFile.name g)
                , StringDict.insert l ((FileName.toString o GenFile.name) g, g))

  fun find (_,l) = StringDict.find l o FileName.toString
  fun findPath (d, l) =
    Option.mapPartial (find (d,l)) o StringDict.find d o FilePath.toUnixPath
  val findName = find
  fun all (_, l) = StringDict.listItems l

  fun join ((d1, l1), (d2, l2)) =
    (StringDict.unionWith #1 (d1, d2), StringDict.unionWith #1 (l1, l2))

  type marker = gdict
  fun mark (src,(d,l)) =
    ( d, StringDict.map (fn x => GenFile.mark (src, x)) l )
  fun createMark (src, (d,l)) =
    ( d, StringDict.map (fn x => GenFile.createMark (src, x)) l )
  fun removeMark (d,l) =
    ( d, StringDict.map GenFile.removeMark l )

end
