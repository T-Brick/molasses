structure StrExport =
struct
  datatype export =
      Str of string * string option
    | Sig of string * string option
    | Fun of string * string option
  type t = export

  local
    val rename =
     fn NONE => ""
      | SOME s => " = " ^ s
    fun rename' d =
     fn NONE => " = " ^ d
      | SOME s => " = " ^ s
  in
    val toString =
     fn Str (s, s2) => "structure " ^ s ^ (rename s2)
      | Sig (s, s2) => "signature " ^ s ^ (rename s2)
      | Fun (s, s2) => "functor " ^ s ^ (rename s2)
    val toReplString =
     fn Str (s, s2) => "structure " ^ s ^ (rename' s s2)
      | Sig (s, s2) => "signature " ^ s ^ (rename' s s2)
      | Fun (s, s2) => "functor " ^ s ^ (rename' s s2)
  end
  val toSimpleString =
    fn Str (s, _) => toString (Str (s, NONE))
     | Sig (s, _) => toString (Sig (s, NONE))
     | Fun (s, _) => toString (Fun (s, NONE))

  val eq =
   fn (Str s1, Str s2) => s1 = s2
    | (Sig s1, Sig s2) => s1 = s2
    | (Fun s1, Fun s2) => s1 = s2
    | _ => false
end
