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
  in
    val toString =
     fn Str (s, s2) => "structure " ^ s ^ (rename s2)
      | Sig (s, s2) => "signature " ^ s ^ (rename s2)
      | Fun (s, s2) => "functor " ^ s ^ (rename s2)
  end

  val eq =
   fn (Str s1, Str s2) => s1 = s2
    | (Sig s1, Sig s2) => s1 = s2
    | (Fun s1, Fun s2) => s1 = s2
    | _ => false
end
