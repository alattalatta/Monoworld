namespace Infusion

open Verse
open UnityEngine

open DefFields

[<AllowNullLiteral>]
type TierDef =
    inherit Def

    val mutable color: Color

    val mutable chances: QualityMap
    val mutable weights: QualityMap

    val mutable maxCount: int

    // used for sorting infusions, higher being higher
    val mutable priority: int

    new() =
        { color = Color.white
          chances = QualityMap()
          weights = QualityMap()
          maxCount = -1
          priority = 0 }
