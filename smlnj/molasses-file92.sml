(* src/file/CMFile.sml : 1.1-187.1 *)
(* molasses-file92.sml *)
structure CMFile :
  sig
    type cmfile
    type t = cmfile

    val group : Source.t option
                  -> FileName.t
                  -> WrappedFile.t list * cmfile list
                  -> cmfile
    val library : Source.t option
                    -> FileName.t
                    -> WrappedFile.t list * cmfile list
                    -> cmfile

    val addExport : cmfile -> StrExport.t -> cmfile
    val addSource : cmfile -> FileName.t -> cmfile

    val addExports : cmfile -> StrExport.t list -> cmfile
    val addSources : cmfile -> FileName.t list -> cmfile

    val restrictExports : cmfile -> StrExport.t list -> cmfile
    val removeExports : cmfile -> cmfile list -> cmfile
    val normalize : cmfile -> cmfile

    include MARKER where type marker = cmfile

    val name : cmfile -> FileName.t
    val toString : cmfile -> string
  end =
  struct
    datatype cmtype = Group | Library

    datatype cmfile =
      CMFile of
               { name : FileName.t
               , cmtype : cmtype
               , exports : StrExport.t list
               , sources : FileName.t list
               }
    | Mark of Source.t * cmfile
    type t = cmfile

    fun app f cmfile =
      case cmfile of
        CMFile r => f r
      | Mark (_, cmfile') => app f cmfile'
    fun map f cmfile =
      case cmfile of
        CMFile r => CMFile (f r)
      | Mark (src, cmfile') => Mark (src, map f cmfile')

    val name : cmfile -> FileName.t = app # name
    val cmtype : cmfile -> cmtype = app # cmtype
    val exports : cmfile -> StrExport.t list = app # exports
    val sources : cmfile -> FileName.t list = app # sources

    fun addExport cmfile export =
      map
        (fn {name, cmtype, exports, sources} =>
           { name = name
           , cmtype = cmtype
           , exports = export :: exports
           , sources = sources
           })
        cmfile
    fun addSource cmfile source =
      map
        (fn {name, cmtype, exports, sources} =>
           { name = name
           , cmtype = cmtype
           , exports = exports
           , sources = source :: sources
           })
        cmfile

    val addExports = List.foldl (fn (e, f) => addExport f e)
    val addSources = List.foldl (fn (e, f) => addSource f e)

    local
      fun changeExports cmfile exp_new f =
        case exp_new of
          [] => cmfile
        | _ =>
            map
              (fn {name, cmtype, exports, sources} =>
                 { name = name
                 , cmtype = cmtype
                 , exports = f exports
                 , sources = sources
                 })
              cmfile
    in
      fun restrictExports cmfile exp_new =
        changeExports cmfile exp_new (fn _ => exp_new)

      fun removeExports cmfile cmfiles =
        let val filter = List.concat (List.map exports cmfiles) in
          changeExports cmfile filter
            (List.filter
               (fn e1 =>
                  not (List.exists (fn e2 => StrExport.eq (e1, e2)) filter)))
        end
    end

    local
      val exports = app # exports
      fun mk ty src_opt fname (wraps, cmfiles) =
        addSources
          (addExports
             ((case src_opt of
                 NONE => (fn x => x)
               | SOME src => (fn x => Mark (src, x)))
                (CMFile {name = fname, cmtype = ty, exports = [], sources = []}))
             (List.concat
                (List.map (WrappedFile.exports) wraps @ List.map exports cmfiles)))
          ((List.map (WrappedFile.name) wraps) @ (List.map name cmfiles))
    in
      val group = mk Group
      val library = mk Library
    end

    local
      fun removeDups eq =
        let
          fun helper (x, acc) =
            if List.exists (fn y => eq (x, y)) acc then
              acc
            else
              x :: acc
        in
          List.foldl helper []
        end
    in
      val normalize =
        map
          (fn {name, cmtype, exports, sources} =>
             { name = name
             , cmtype = cmtype
             , exports = removeDups StrExport.eq exports
             , sources = removeDups FileName.eq sources
             })
    end

    type marker = cmfile
    val mark = Mark
    fun createMark (src, cmfile) =
      case cmfile of
        Mark _ => cmfile
      | CMFile r => Mark (src, cmfile)
    fun removeMark cmfile =
      case cmfile of
        Mark (_, cmfile') => removeMark cmfile'
      | CMFile _ => cmfile

    fun sourceToExportString source =
      (if FileName.isSML source then
         "source("
       else
         "library(")
      ^ FileName.toString source
      ^ ")"

    fun toString cmfile =
      case cmfile of
        CMFile {name, cmtype, exports, sources} =>
          (case cmtype of
             Group => "Group\n\t"
           | Library =>
               case exports of
                 [] =>
                   ("Library\n\t" ^ (String.concatWith "\n\t"
                                       (List.map sourceToExportString
                                          (List.rev sources))))
               | _ => "Library\n\t")
          ^ (String.concatWith "\n\t"
               (List.map StrExport.toSimpleString (List.rev exports)))
          ^ "\nis\n\t"
          ^ (String.concatWith "\n\t"
               (List.map FileName.toString (List.rev sources)))
      | Mark (src, cmfile') =>
          "(* "
          ^ (Source.toRegionString src)
          ^ " *)\n"
          ^ (toString cmfile')
  end

