
structure MolassesExt =
struct
  val _ = Tools.registerClassifier (
		Tools.stdSfxClassifier { sfx = "mlb", class = "molasses" })
end
