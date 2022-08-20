structure CMGenerator :> INTERNAL_GENERATOR =
struct
  structure WFile = WrappedFile

  exception Unsupported of string
  exception IllegalState

  type output =
    { all : GenFile.t list
    , cm : CMFile.t option
    , toplevel : FileName.t * Import.t list
    }

  type acc =
    { cms : CMFile.t list
    , smls : WFile.t list
    , future : WFile.future
    , filter : StrExport.t list * bool (* export list, does rename *)
    , gened : Generated.t
    }
  val emptyAcc =
    { cms = []
    , smls = []
    , future = WFile.initFuture
    , filter = ([], false)
    , gened = Generated.empty
    }

  fun isAccEmpty ({cms, smls, ...} : acc) =
    case (cms, smls) of
      ([], []) => true
    | _ => false

  fun createMark (source, {cms, smls, future, filter, gened} : acc) =
    { cms = List.map (fn x => CMFile.createMark (source, x)) cms
    , smls = List.map (fn x => WFile.createMark (source, x)) smls
    , future = future
    , filter =
        ( List.map (fn x => StrExport.createMark (source, x)) (#1 filter)
        , #2 filter )
    , gened = Generated.createMark (source, gened)
    }

  fun getFuture ({future, ...} : acc) = future
  fun getGened ({gened, ...} : acc) = gened
  fun getAll ({gened, ...} : acc) = Generated.all gened

  fun foundSMLCache (acc : acc) (g : WFile.t) =
    { cms = #cms acc, smls = g :: #smls acc, future = #future acc
    , filter = #filter acc, gened = #gened acc }
  fun foundCMCache (acc : acc) (g : CMFile.t) =
    { cms = g :: #cms acc, smls = #smls acc, future = #future acc
    , filter = #filter acc, gened = #gened acc }

  fun foundLibrary (acc : acc) (wfile : WFile.t) =
    { cms = #cms acc, smls = wfile :: #smls acc, future = #future acc
    , filter = #filter acc, gened = #gened acc }

  fun makeSML (dir, pathmap) (acc : acc) create result =
    let
      val wfile = WFile.make (#future acc) result
    in
      { cms = #cms acc
      , smls = wfile :: #smls acc
      , future = WFile.futureImports wfile
      , filter = #filter acc
      , gened = Generated.insert (#gened acc)
                  (SOME result, GenFile.SML wfile)
      }
    end
  fun makeMLB (_, pathmap) (acc : acc) create result =
    let
      val (dir, MLBAst.Ast basdec) = GeneratorUtil.getAst result
    in
      create (dir, pathmap) (SOME result) acc basdec
    end

  fun makeFilter (acc : acc) kind vals =
    { cms = #cms acc
    , smls = #smls acc
    , future = #future acc (* TODO Change I think *)
    , filter =
        List.foldr (fn ((t, t_opt), (l,b)) =>
          ( kind (MLBToken.toString t, Option.map MLBToken.toString t_opt) :: l
          , b orelse Option.isSome t_opt )
        ) (#filter acc) vals
    , gened = #gened acc
    }

  fun makeList (fs as (dir, pathmap)) cfile tok_opt create acc file elems =
    let
      fun folder ((d, src_opt), a) =
        ( (case src_opt of
              NONE => (fn x => x)
            | SOME src => (fn x => createMark (src, x))
          ) o create fs NONE a ) d
      val {cms, smls, future, filter, gened} = List.foldl folder acc elems
      (* remove duplicate entries in the export lists *)
      val corrected_cms = cms
        (* List.foldr (fn (c, acc) => (CMFile.removeExports c acc)::acc) [] cms *)
      val corrected_gened = gened
        (* List.foldl (fn (c,g) =>
            Generated.insert g (NONE, GenFile.CM c)
         ) gened cms *)
      val cm = CMFile.normalize
        ( CMFile.restrictExports
            (CMFile.library tok_opt file (smls, corrected_cms))
            (#1 filter)
        )
      val () = Control.print ("Finished: " ^ FileName.toString file ^ "\n")
      fun noFilter () : acc =
        { cms = [cm]
        , smls = []
        , future = future
        , filter = filter
        , gened = Generated.insert corrected_gened (cfile, GenFile.CM cm)
        }
      fun appFilter () : acc =
        let
          val rename_file = FileName.newSML ()
          val cm' = CMFile.addSource cm rename_file
          val rename = GenFile.Rename (rename_file, #1 filter)
        in
          { cms = [cm']
          , smls = []
          , future = future
          , filter = ([], false)
          , gened = Generated.insert
                      (Generated.insert corrected_gened (NONE, rename))
                      (cfile, GenFile.CM cm')
          }
        end
    in
      case filter of
        (_, false) => noFilter ()
      | ([], _)    => noFilter ()
      | (_, true)  => appFilter ()
    end

  (* Idea - split local into 2 CM files: all the information prior to the
   * local dec and all the parts in the local declaration. Then wrap all
   * these into a new CM file, referencing the two CM files we just def'd
   *)
  fun makeLocal (dir,pathmap) cfile create acc
                (locall,inn,endd) (basdec1,basdec2) =
    let
      val new_cm = FileName.newCM ()
      val (old_cm, new_acc) =
        if isAccEmpty acc
        then (new_cm, acc)
        else
          let
            val old = FileName.newCM ()
          in
            (old, makeList (dir, pathmap) cfile NONE create acc old [])
          end
      val {cms, smls, future, filter, gened} =
        createMark
          ( MLBToken.getSource endd
          , makeList (dir, pathmap) NONE ((SOME o MLBToken.getSource) endd)
              create new_acc (new_cm)
              [ (basdec1, (SOME o MLBToken.getSource) locall)
              , (basdec2, (SOME o MLBToken.getSource) inn) ]
          )
    in
      case Generated.findName gened old_cm of
        SOME (GenFile.CM g) =>
          { cms = g::cms, smls = smls, future = future
          , filter = ([], false), gened = gened }
      | _ => raise Fail "Couldn't find file that should be generated..."
    end
end
