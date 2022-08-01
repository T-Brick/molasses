structure StrExport =
struct
  datatype export =
      Str of string
    | Sig of string
    | Fun of string
  type t = export

  val toString =
   fn Str s => "structure " ^ s
    | Sig s => "signature " ^ s
    | Fun s => "functor " ^ s
  val eq =
   fn (Str s1, Str s2) => s1 = s2
    | (Sig s1, Sig s2) => s1 = s2
    | (Fun s1, Fun s2) => s1 = s2
    | _ => false
end
