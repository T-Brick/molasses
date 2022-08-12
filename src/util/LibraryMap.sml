structure LibraryMap =
struct
  (* basic hack, clean-up later *)
  val convert : string -> string =
   fn "$(SML_LIB)/basis/basis.mlb" => "$/basis.cm"
    | "$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb" => "$smlnj-lib.cm"
    | "$(SML_LIB)/basis/mlton.mlb" => "$/basis.cm (* mlton.mlb *)"
    | "$(SML_LIB)/basis/unsafe.mlb" => "$/basis.cm"
    | "$(SML_LIB)/basis/build/sources.mlb" => ""
    | s => raise Fail ("Unknown library file!\t" ^ s)
end
