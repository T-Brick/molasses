(* src/control/LibraryMap.sml : 1.1-78.1 *)
(* molasses-file80.sml *)
structure LibraryMap :>
  sig
    type libmap
    type t = libmap

    exception LibraryNotFound of string

    val empty : libmap
    val default : libmap

    val isLibraryPathVar : libmap -> string -> bool
    val convert : libmap -> string -> string

    val addMappings : libmap -> (string * string) list -> libmap
    val addPathVars : libmap -> string list -> libmap

    val addFromString : libmap -> string -> libmap
  end =
  struct

    structure Map =
      Dict
        (type t = string
         val compare = String.compare)

    (* the conversions, library path variables *)
    type libmap = string Map.t * string list
    type t = libmap

    exception LibraryNotFound of string

    val empty = (Map.empty, [])

    (* some of these could potentially be inaccurate *)
    val default =
      ( Map.fromList
          [ ("$(SML_LIB)/basis/basis.mlb", "$/basis.cm")
          , ("$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb", "$/smlnj-lib.cm")
          , ("$(SML_LIB)/basis/mlton.mlb", "$/basis.cm (* mlton *)")
          , ("$(SML_LIB)/basis/unsafe.mlb", "$/basis.cm (* unsafe *)")
          , ("$(SML_LIB)/basis/build/sources.mlb", "$/basis.cm (* build *)")
          ]
      , ["SML_LIB"]
      )

    fun isLibraryPathVar (_, pathvars) s =
      List.exists (fn pv => s = pv) pathvars

    fun convert (conversions, _) s =
      Map.lookup conversions s
        handle Map.NotFound => raise LibraryNotFound ("'" ^ s ^ "'")

    fun addMappings (conversions, pathvars) ms =
      case ms of
        [] => (conversions, pathvars)
      | m :: ms' => addMappings (Map.insert conversions m, pathvars) ms'

    fun addPathVars (conversions, pathvars) pvs = (conversions, pvs @ pathvars)

    local
      infix |>
      fun x |> f = f x

      exception LibraryMapParseError

      val isNewLine = fn #"\n" => true | _ => false
      val isTab = fn #" " => true | _ => false
      fun tuplify vs =
        case vs of
          [] => raise LibraryMapParseError
        | x :: [] => raise LibraryMapParseError
        | _ => (List.hd vs, String.concatWith " " (List.tl vs))
    in
      fun addFromString libmap str =
        str
        |> String.tokens isNewLine
        |> List.map (String.tokens isTab)
        |> (fn l =>
              if List.length l = 0 then
                raise LibraryMapParseError
              else
                (List.map tuplify (List.tl l), List.hd l))
        |> (fn (m, p) => addPathVars (addMappings libmap m) p)
    end

  end

