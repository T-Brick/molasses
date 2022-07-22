signature EITHER_GLOBAL =
  sig
    datatype ('a, 'b) either = Left of 'a | Right of 'b
    val &  : ('a -> 'c) * ('b -> 'c) -> ('a, 'b) either -> 'c
    val && : ('a -> 'c) * ('b -> 'd) -> ('a, 'b) either -> ('c, 'd) either
  end

signature EITHER =
  sig
    include EITHER_GLOBAL
    val isLeft  : ('a, 'b) either -> bool
    val isRight : ('a, 'b) either -> bool
    (* ... *)
  end
