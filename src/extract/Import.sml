structure Import =
struct
  datatype import =
      Open of InternalStruct.t
    | Infix of int option * string list
    | Infixr of int option * string list
    | Nonfix of string list
  type t = import

  local
    fun infixToString kind fns = fn
      NONE => kind ^ (String.concatWith " " fns)
    | SOME p => kind ^ (Int.toString p) ^ " " ^ (String.concatWith " " fns)
  in
    val toString =
     fn Open str => "open " ^ InternalStruct.toString str
      | Infix (p_opt, fns) => infixToString "infix " fns p_opt
      | Infixr (p_opt, fns) => infixToString "infixr " fns p_opt
      | Nonfix fns => infixToString "nonfix " fns NONE
  end
end
