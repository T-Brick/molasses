structure ExpExport =
struct
  datatype export =
      Val of Ast.Pat.t
    | Fun of string
    | Type of Ast.Exp.typbind
    | Datatype of string
    | Exn of string
    | Open of string
  type t = export

  local
    fun pp show =
      TerminalColorString.toString {colors = false}
      o StringDoc.toString
      o TokenDoc.toStringDoc {tabWidth = 0}
      o show

    fun mkType ty =
      let
        val pp = (
          case PrettyUtil.maybeShowSyntaxSeq (#tyvars ty) (TokenDoc.token) of
              NONE => ""
            | SOME s => pp (fn x => x) s ^ " "
          ) ^ (Token.toString (#tycon ty))
      in
        pp ^ " = " ^ pp
      end

  in
    val toString =
     fn Val p =>
          "val " ^ (pp PrettyPat.showPat p) ^ " = " ^ (pp PrettyPat.showPat p)
      | Fun s => "val " ^ s ^ " = " ^ s
      | Type t => "type " ^ (
          String.concatWith " and " (Seq.toList (Seq.map mkType (#elems t)))
      )
      | Datatype s => "datatype " ^ s ^ " = datatype " ^ s
      | Exn s => "exception " ^ s ^ " = " ^ s
      | Open s => "open " ^ s
  end
end
