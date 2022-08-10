structure CMFile : sig
  type cmfile
  type t = cmfile

  val group   : FileName.t -> WrappedFile.t list * cmfile list -> cmfile
  val library : FileName.t -> WrappedFile.t list * cmfile list -> cmfile

  val addExport : cmfile -> StrExport.t -> cmfile
  val addSource : cmfile -> FileName.t  -> cmfile

  val addExports : cmfile -> StrExport.t list -> cmfile
  val addSources : cmfile -> FileName.t list  -> cmfile

  val restrictExports : cmfile -> StrExport.t list -> cmfile
  val normalize : cmfile -> cmfile

  val name : cmfile -> FileName.t
  val toString : cmfile -> string
end = struct
  datatype cmtype =
      Group
    | Library

  type cmfile = (FileName.t * cmtype * StrExport.t list * FileName.t list)
  type t = cmfile

  fun name (name, _, _, _) = name

  fun addExport (name, ty, exports, sources) export =
    (name, ty, export::exports, sources)
  fun addSource (name, ty, exports, sources) source =
    (name, ty, exports, source::sources)

  val addExports = List.foldl (fn (e, f) => addExport f e)
  val addSources = List.foldl (fn (e, f) => addSource f e)

  fun restrictExports (name, ty, exp_old, sources) exp_new =
    case exp_new of
      [] => (name, ty, exp_old, sources)
    | _  => (name, ty, exp_new, sources)

  local
    fun exports (_, _, exports, _) = exports
    fun mk ty fname (wraps, cmfiles) =
      addSources
        ( addExports
            (fname, ty, [], [])
            ( List.concat
                ( List.map (WrappedFile.exports) wraps
                @ List.map exports cmfiles )
            ) )
        ( (List.map (WrappedFile.name) wraps) @ (List.map name cmfiles) )
  in
    val group   = mk Group
    val library = mk Library
  end

  local
    fun removeDups eq =
      let
        fun helper (x, acc) =
          if List.exists (fn y => eq (x, y)) acc
          then acc
          else x::acc
      in
        List.foldl helper []
      end
  in
    fun normalize (name, ty, exports, sources) =
      ( name
      , ty
      , removeDups StrExport.eq exports
      , removeDups FileName.eq sources
      )
  end

  fun toString (name, ty, exports, sources) =
    (case ty of
        Group => "Group\n\t"
      | Library =>
        case exports of
          [] => (
            print "Library with no exports, converting to group...";
            "Group\n\t"
          )
        | _ => "Library\n\t"
    ) ^ (String.concatWith
          "\n\t"
          (List.map StrExport.toSimpleString (List.rev exports))
        )
      ^ "\nis\n\t"
      ^ (String.concatWith
          "\n\t"
          (List.map FileName.toString (List.rev sources))
        )
end
