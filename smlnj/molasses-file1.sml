(* parse-sml/src/base/lib/compat/nj-array.sml : 1.1-14.1 *)
(* molasses-file1.sml *)
structure Array =
  struct
    open Array

    fun alloc (n : int) : 'a array = Unsafe.cast (array (n, 0))
  end

structure Word8Array =
  struct
    open Word8Array

    fun alloc (n : int) : array = array (n, Word8.fromInt 0)
  end

