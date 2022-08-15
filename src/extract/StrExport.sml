structure StrExport =
struct
  datatype export =
      Str of string * string option
    | Sig of string * string option
    | Fun of string * string option
    | Mark of Source.t * export
  type t = export

  type marker = export
  val mark = Mark
  fun createMark (src, export) =
    case export of
      Mark _ => export
    | _ => Mark (src, export)
  fun removeMark export =
    case export of
      Mark (_, export') => removeMark export'
    | _ => export

  local
    val rename =
     fn NONE => ""
      | SOME s => " = " ^ s
  in
    fun toString e =
      case e of
        Str (s, s2) => "structure " ^ s ^ (rename s2)
      | Sig (s, s2) => "signature " ^ s ^ (rename s2)
      | Fun (s, s2) => "functor " ^ s ^ (rename s2)
      | Mark (src, e') =>
          toString e' ^ "\t(* " ^ Source.toRegionString src ^ " *)"
    fun toRenameString e =
      case e of
        Str (_, SOME _) => toString e
      | Str (_, NONE) => ""
      | Sig (_, SOME _) => toString e
      | Sig (_, NONE) => ""
      | Fun (_, SOME _) => toString e
      | Fun (_, NONE) => ""
      | Mark (src, e') =>
          toRenameString e' ^ "\t(* " ^ Source.toRegionString src ^ " *)"
    fun toSimpleString e =
      case e of
        Str (s, _) => toString (Str (s, NONE))
      | Sig (s, _) => toString (Sig (s, NONE))
      | Fun (s, _) => toString (Fun (s, NONE))
      | Mark (src, e') =>
          toSimpleString e' ^ "\t(* " ^ Source.toRegionString src ^ " *)"
  end

  fun eq (e1, e2) =
    case (e1, e2) of
      (Str s1, Str s2) => s1 = s2
    | (Sig s1, Sig s2) => s1 = s2
    | (Fun s1, Fun s2) => s1 = s2
    | (Mark (_, e1'), _) => eq (e1', e2)
    | (_, Mark (_, e2')) => eq (e1, e2')
    | _ => false
end
