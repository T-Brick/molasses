(* parse-sml/src/base/lib/compat/nj-word16.sml : 1.1-8.1 *)
(* molasses-file5.sml *)
(* extremely hacky, make this proper later *)
structure Word16 =
  struct
    open Word32

    val wordSize = 16
  end

