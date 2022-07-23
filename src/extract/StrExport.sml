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
end
