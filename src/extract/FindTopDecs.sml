structure FindTopDecs : sig
  val find : Ast.t -> (Export.t list * Import.t list)
end = struct
  open Ast

  structure I = Import
  structure E = Export

  val appendBoth = fn ((a,b),(x,y)) => (a @ x, b @ y)

  fun getFun fname_args =
    E.Fun (
      case fname_args of
        Exp.PrefixedFun r => Token.toString (#id r)
      | Exp.InfixedFun r => "(op " ^ (Token.toString (#id r)) ^ ")"
      | Exp.CurriedInfixedFun r => "(op " ^ (Token.toString (#id r)) ^ ")"
    )
  fun getExn exbind =
    (E.Exn o Token.toString) (
      case exbind of
        Exp.ExnNew r => #id r
      | Exp.ExnReplicate r => #left_id r
    )

  fun findExpDec expdec =
    case expdec of
      Exp.DecEmpty => ([], [])
    | Exp.DecVal r => (Seq.toList (Seq.map (E.Val o #pat) (#elems r)),[])
    | Exp.DecFun r =>
        (Seq.toList (
          Seq.map
            (getFun o #fname_args o (fn s => Seq.nth s 0) o #elems)
            ((#elems o #fvalbind) r)
        ), [])
    | Exp.DecType r => ([E.Type (#typbind r)], [])
    | Exp.DecDatatype r =>
        (Seq.toList (
          Seq.map (E.Datatype o Token.toString o #tycon) ((#elems o #datbind) r)
        ), [])
    | Exp.DecReplicateDatatype r =>
        ([(E.Datatype o Token.toString o #left_id) r], [])
    | Exp.DecAbstype r => raise Fail "Abstype Not Implemented!!!"
    | Exp.DecException r => (Seq.toList (Seq.map getExn (#elems r)), [])
    | Exp.DecLocal r => findExpDec (#right_dec r)
    | Exp.DecOpen r =>
        (Seq.toList (
          Seq.map (E.Open o Token.toString o MaybeLongToken.getToken) (#elems r)
        ), [])
    | Exp.DecMultiple r =>
        Seq.reduce appendBoth ([], []) (Seq.map findExpDec (#elems r))
    | Exp.DecInfix r => ([],
        [ I.Infix
          ( Option.mapPartial (Int.fromString o Token.toString) (#precedence r)
          , Seq.toList (Seq.map Token.toString (#elems r))
          )
        ])
    | Exp.DecInfixr r => ([],
        [ I.Infixr
          ( Option.mapPartial (Int.fromString o Token.toString) (#precedence r)
          , Seq.toList (Seq.map Token.toString (#elems r))
          )
        ])
    | Exp.DecNonfix r =>
        ([], [I.Nonfix (Seq.toList (Seq.map Token.toString (#elems r)))])

  fun findStrDec strdec =
    case strdec of
      Str.DecEmpty => ([], [])
    | Str.DecCore d => findExpDec d
    | Str.DecStructure r => ([], [])
    | Str.DecMultiple r =>
        Seq.reduce appendBoth ([], []) (Seq.map findStrDec (#elems r))
    | Str.DecLocalInEnd r => findStrDec (#strdec2 r)
    | Str.MLtonOverload r => raise Fail "Unimplemented"

  fun findTopDec topdec =
    case topdec of
      StrDec strdec => findStrDec strdec
    | _ => ([], [])

  fun find (Ast topdecs) =
    Seq.reduce appendBoth ([], []) (Seq.map (findTopDec o #topdec) topdecs)
end
