(* This is the top-level environment used when creating the NJ heap image *)
val () = #set Molasses.Control.recover_src true
val use = MolassesNJ.use
