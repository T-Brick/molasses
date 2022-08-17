val x = 0
val (y) = 0

val (t1, t2, t3, t4, t5) = (1, 2, 3, 4, 5)
val ((nt1, (nt2, nt3), nt4), nt5) = ((1, (2, 3), 4), 5)

val {r1, r2, r3} = {r1 = 1, r2 = 2, r3 = 3}
val {r1', r2', ...} = {r1' = 1, r2' = 2, r3' = 3}

val () = ()

val SOME c1 = SOME 1
val SOME (c1', c2') = SOME (1,2)

val [l1, l2, l3, l4] = [1,2,3,4]

val _ = 1
val (_, wt2, wt3) = (1, 2, 3)
