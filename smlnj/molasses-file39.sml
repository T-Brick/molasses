(* parse-sml/src/base/WithSource.sml : 1.1-29.1 *)
(* molasses-file39.sml *)
(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure WithSource :>
  sig
    type 'a t

    val make : {value : 'a, source : Source.t} -> 'a t
    val unpack : 'a t -> {value : 'a, source : Source.t}

    val valOf : 'a t -> 'a
    val srcOf : 'a t -> Source.t

    val map : ('a -> 'b) -> 'a t -> 'b t
  end =
  struct
    type 'a t = {value : 'a, source : Source.t}
    fun make x = x
    fun unpack x = x

    fun valOf {value, source} = value
    fun srcOf {value, source} = source

    fun map f {value, source} = make {value = f value, source = source}
  end

