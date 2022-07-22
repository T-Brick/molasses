structure Either : EITHER =
  struct
    datatype ('a, 'b) either = Left of 'a | Right of 'b
    fun f & g = fn x =>
      case x of Left z => f z | Right z => g z
    fun f && g = (Left o f) & (Right o g)
    fun isLeft x = ((fn _ => true) & (fn _ => false)) x
    fun isRight x = (not o isLeft) x
    (* ... *)
  end
structure EitherGlobal : EITHER_GLOBAL = Either
