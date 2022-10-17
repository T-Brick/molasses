(* parse-sml/src/base/lib/compat/nj.sml : 1.1-59.1 *)
(* molasses-file4.sml *)
(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure ForkJoin :
  sig
    val par : (unit -> 'a) * (unit -> 'b) -> 'a * 'b
    val parfor : int -> int * int -> (int -> unit) -> unit
    val alloc : int -> 'a array
  end =
  struct
    fun par (f, g) = (f (), g ())
    fun parfor (g : int) (lo, hi) (f : int -> unit) =
      if lo >= hi then
        ()
      else
        (f lo; parfor g (lo + 1, hi) f)
    fun alloc n = ArrayExtra.alloc n
  end

structure Concurrency =
  struct
    val numberOfProcessors = 1

    fun cas r (x, y) =
      let val current = ! r in
        if MLton.eq (x, current) then r := y else (); current
      end

    fun casArray (a, i) (x, y) =
      let val current = Array.sub (a, i) in
        if MLton.eq (x, current) then Array.update (a, i, y) else (); current
      end
  end

structure ReadFile = PosixReadFile

structure GCStats : sig val report : unit -> unit end =
  struct

    fun p name thing = print (name ^ ": " ^ thing () ^ "\n")

    fun report () =
      let in print ("======== GC Stats ========\n"); print "none yet...\n" end

  end

