(* This is the top-level environment used when creating the NJ heap image *)
val () = #set Molasses.Control.recover_src true
val () = #set Molasses.Control.kill_on_parse_err false
val use = MolassesNJ.use
