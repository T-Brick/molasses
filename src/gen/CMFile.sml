structure CMFile : sig
  type cmfile
  type t = cmfile

  val group   : FileName.t -> WrappedFile.t list -> cmfile
  val library : FileName.t -> WrappedFile.t list -> cmfile

  val addExport : cmfile -> StrExport.t -> cmfile
  val addSource : cmfile -> FileName.t  -> cmfile

  val addExports : cmfile -> StrExport.t list -> cmfile
  val addSources : cmfile -> FileName.t list  -> cmfile

  val restrictExports : cmfile -> StrExport.t list -> cmfile

  val name : cmfile -> FileName.t
  val toString : cmfile -> string
end = struct
  datatype cmtype =
      Group
    | Library

  type cmfile = (FileName.t * cmtype * StrExport.t list * FileName.t list)
  type t = cmfile

  fun addExport (name, ty, exports, sources) export =
    (name, ty, export::exports, sources)
  fun addSource (name, ty, exports, sources) source =
    (name, ty, exports, source::sources)

  val addExports = List.foldl (fn (e, f) => addExport f e)
  val addSources = List.foldl (fn (e, f) => addSource f e)

  fun restrictExports (name, ty, _, sources) exports =
    (name, ty, exports, sources)

  local
    fun mk ty name wraps =
      addSources
        ( addExports
            (name, ty, [], [])
            (List.concat (List.map (WrappedFile.exports) wraps)) )
        (List.map (WrappedFile.name) wraps)
  in
    val group   = mk Group
    val library = mk Library
  end

  fun name (name, _, _, _) = name
  fun toString (name, ty, exports, sources) =
    (case ty of
        Group => "Group\n\t"
      | Library => "Library\n\t"
    ) ^ (String.concatWith
          "\n\t"
          (List.map StrExport.toString (List.rev exports))
        )
      ^ "\nis\n\t"
      ^ (String.concatWith
          "\n\t"
          (List.map FileName.toString (List.rev sources))
        )
end
