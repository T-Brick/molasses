structure GenFile : sig
  type rename = FileName.t * StrExport.t list
  type toplevel = FileName.t * Import.t list

  datatype generated =
      CM of CMFile.t
    | SML of WrappedFile.t
    | Rename of rename
    | TopLevel of toplevel
  type t = generated

  val apply : (CMFile.t -> 'a)
            * (WrappedFile.t -> 'a)
            * (rename -> 'a)
            * (toplevel -> 'a)
            -> generated
            -> 'a
  val map : (CMFile.t -> CMFile.t)
          * (WrappedFile.t -> WrappedFile.t)
          * (rename -> rename)
          * (toplevel -> toplevel)
          -> generated
          -> generated

  include MARKER where type marker = generated

  val isSML : generated -> bool
  val isCM : generated -> bool
  val isRename : generated -> bool
  val isTopLevel : generated -> bool

  val name : generated -> FileName.t
  val toString : generated -> string
end = struct
  type rename = FileName.t * StrExport.t list
  type toplevel = FileName.t * Import.t list

  datatype generated =
      CM of CMFile.t
    | SML of WrappedFile.t
    | Rename of rename
    | TopLevel of toplevel
  type t = generated

  fun apply (cf,sf,rf,tf) g =
    case g of
      CM cm => cf cm
    | SML wf => sf wf
    | Rename r => rf r
    | TopLevel t => tf t
  fun map (cf,sf,rf,tf) =
    apply (CM o cf, SML o sf, Rename o rf, TopLevel o tf)

  val isSML      = fn SML _      => true | _ => false
  val isCM       = fn CM _       => true | _ => false
  val isRename   = fn Rename _   => true | _ => false
  val isTopLevel = fn TopLevel _ => true | _ => false

  val name =
    apply (CMFile.name, WrappedFile.name, fn (p,_) => p, fn (p,_) => p)
  val toString =
    apply ( CMFile.toString
          , WrappedFile.toString
          , fn (_, v) => String.concatWith "\n"
                            (List.map StrExport.toRenameString v)
          , fn (_, v) => String.concatWith "\n"
                            (List.map Import.toString v)
          )

  type marker = generated
  fun mark (src, g) =
    map ( fn cm => CMFile.mark (src, cm)
        , fn wf => WrappedFile.mark (src, wf)
        , fn x => x
        , fn x => x
        ) g
  fun createMark (src, g) =
    map ( fn cm => CMFile.createMark (src, cm)
        , fn wf => WrappedFile.createMark (src, wf)
        , fn x => x
        , fn x => x
        ) g
  fun removeMark g =
    map (CMFile.removeMark, WrappedFile.removeMark, fn x => x, fn x => x) g
end
