(* src/util/SafeParser.sml : 1.1-29.1 *)
(* molasses-file83.sml *)
structure SafeParser =
  struct

    local
      fun handleLexOrParseError exn =
        let
          val e =
            case exn of
              Error.Error e => e
            | other => raise other
          val hist = MLton.Exn.history exn
        in
          TerminalColorString.print
            (Error.show {highlighter = SOME SyntaxHighlighter.fuzzyHighlight} e);
          if List.null hist then
            ()
          else
            print ("\n" ^ String.concat (List.map (fn ln => ln ^ "\n") hist));
          OS.Process.exit OS.Process.failure
        end
    in
      fun parse source =
        Parser.parse source handle exn => handleLexOrParseError exn

      fun parseWithInfdict fixities source =
        Parser.parseWithInfdict fixities source
          handle exn => handleLexOrParseError exn
    end
  end

