(* src/extract/ExpExport.sml : 1.1-113.1 *)
(* molasses-file87.sml *)
structure ExpExport =
  struct
    datatype export =
      Val of Ast.Pat.t
    | Fun of string
    | Type of Ast.Exp.typbind
    | Datatype of string
    | Exn of string
    | Open of string
    | Mark of Source.t * export
    type t = export

    type marker = export
    val mark = Mark
    fun createMark (src, export) =
      case export of
        Mark _ => export
      | _ => Mark (src, export)
    fun removeMark export =
      case export of
        Mark (_, export') => removeMark export'
      | _ => export

    local
      fun pp show =
        TerminalColorString.toString {colors = false}
        o StringDoc.toString
        o TokenDoc.toStringDoc {tabWidth = 0}
        o show

      fun mkType (ty : { tyvars : Token.t Ast.SyntaxSeq.t
                       , tycon : Token.t
                       , eq : Token.t
                       , ty : Ast.Ty.t
                       }) =
        let
          val pp =
            (case PrettyUtil.maybeShowSyntaxSeq (# tyvars ty) (TokenDoc.token) of
               NONE => ""
             | SOME s => pp (fn x => x) s ^ " ") ^ (Token.toString (# tycon ty))
        in
          pp ^ " = " ^ pp
        end

      fun extractVars pat acc =
        let open Ast.Pat in
          case pat of
            Wild tok => acc
          | Const tok => acc
          | Unit {left, right} => acc
          | Ident {opp, id} =>
              ((case opp of
                  NONE => ""
                | SOME _ => "op ") ^ (Token.toString o MaybeLongToken.getToken)
                                       id) :: acc
          | Parens {left, pat, right} => extractVars pat acc
          | Tuple {left, elems, delims, right} =>
              List.foldl (fn (p, a) => extractVars p a) acc (Seq.toList elems)
          | List {left, elems, delims, right} =>
              List.foldl (fn (p, a) => extractVars p a) acc (Seq.toList elems)
          | Record {left, elems, delims, right} =>
              let
                fun showPatRow patrow acc =
                  case patrow of
                    DotDotDot ddd => acc
                  | LabEqPat {lab, eq, pat} => extractVars pat acc
                  | LabAsPat {id, ty, aspat} =>
                      (case aspat of
                         NONE => (Token.toString id :: acc)
                       | SOME {ass, pat} =>
                           extractVars pat (Token.toString id :: acc))
              in
                List.foldl (fn (p, a) => showPatRow p a) acc (Seq.toList elems)
              end
          | Con {opp, id, atpat} => extractVars atpat acc
          | Typed {pat, colon, ty} => extractVars pat acc
          | Layered {opp, id, ty, ass, pat} =>
              extractVars pat
                (((case opp of
                     NONE => ""
                   | SOME _ => "op ") ^ Token.toString id) :: acc)
          | Infix {left, id, right} => extractVars right (extractVars left acc)
        end
    in
      fun toString e =
        case e of
          Val p =>
            (* "val " ^ (pp PrettyPat.showPat p) ^ " = " ^ (pp PrettyPat.showPat p) *)
            let
              fun mkVal x = "val " ^ x ^ " = " ^ x
              val vars = List.rev (List.map mkVal (extractVars p []))
            in
              String.concatWith "\t" vars
            end
        | Fun s => "val " ^ s ^ " = " ^ s
        | Type t =>
            "type " ^ (String.concatWith " and "
                         (Seq.toList (Seq.map mkType (# elems t))))
        | Datatype s => "datatype " ^ s ^ " = datatype " ^ s
        | Exn s => "exception " ^ s ^ " = " ^ s
        | Open s => "open " ^ s
        | Mark (s, e') =>
            (toString e')
            ^ "\t(* "
            ^ (Source.toRegionString s)
            ^ " *)"
    end
  end

