structure LibraryMap =
struct
  (* basic hack, clean-up later *)
  val convert : string -> string =
   fn "$(SML_LIB)/basis/basis.mlb" => "$/basis.cm"
    | _ => raise Fail "Unknown library file!"
end
