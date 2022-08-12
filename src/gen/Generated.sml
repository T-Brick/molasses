structure Generated :> sig
  datatype generated =
      CM of CMFile.t
    | SML of WrappedFile.t
    | Rename of FileName.t * StrExport.t list
  type t = generated

  type gdict

  exception NotFound

  val empty : gdict
  val isEmpty : gdict -> bool
  val singleton : (FilePath.t option * generated) -> gdict
  val size : gdict -> int
  val sizeAll : gdict -> int
  val insert : gdict -> (FilePath.t option * generated) -> gdict

  val find : gdict -> FilePath.t -> generated option
  val all : gdict -> generated list

  val join : gdict * gdict -> gdict
end = struct
  datatype generated =
      CM of CMFile.t
    | SML of WrappedFile.t
    | Rename of FileName.t * StrExport.t list
  type t = generated

  structure StringDict = Dict(
    type t = string
    val compare = String.compare
  )

  val genToString =
   fn CM f => (FileName.toString o CMFile.name) f
    | SML f => (FileName.toString o WrappedFile.name) f
    | Rename (f, _) => FileName.toString f

  exception NotFound = StringDict.NotFound

  (* first is all real files, second is all generated files *)
  type gdict = generated StringDict.t * generated StringDict.t

  val empty = (StringDict.empty, StringDict.empty)
  fun isEmpty (_, l) = StringDict.isEmpty l
  fun size (d, _) = StringDict.size d
  fun sizeAll (_, l) = StringDict.size l
  fun singleton (fopt, g) =
    case fopt of
      NONE => (StringDict.empty, StringDict.singleton (genToString g, g))
    | SOME f => ( StringDict.singleton (FilePath.toUnixPath f, g)
                , StringDict.singleton (genToString g, g)
                )
  fun insert (d,l) (fopt, g) =
    case fopt of
      NONE => (d, StringDict.insert l (genToString g, g))
    | SOME f => ( StringDict.insert d (FilePath.toUnixPath f, g)
                , StringDict.insert l (genToString g, g))

  fun find (d, _) f = StringDict.find d (FilePath.toUnixPath f)
  fun all (_, l) = StringDict.listItems l

  fun join ((d1, l1), (d2, l2)) =
    (StringDict.unionWith #1 (d1, d2), StringDict.unionWith #1 (l1, l2))

end
