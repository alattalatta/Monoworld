namespace Infusion

open UnityEngine

open DefFields

type TierDef =
    inherit HashEqualDef

    val mutable color: Color

    val mutable chances: QualityMap
    val mutable weights: QualityMap

    val mutable maxCount: int

    // used for sorting infusions, higher being higher
    val mutable priority: int

    // should generator generate an infuser for this tier?
    val mutable infusable: bool
    // market value of the generated infuser
    val mutable infuserValue: float32
    // extraction success chance
    val mutable extractionChance: float32

    new() =
        { inherit HashEqualDef()
          color = Color.white
          chances = QualityMap()
          weights = QualityMap()
          maxCount = -1
          priority = 0
          infusable = true
          infuserValue = 100.0f
          extractionChance = 1.0f }

module TierDef =
    let empty = TierDef()

    let isEmpty (tier: TierDef) = tier.defName = "UnnamedDef"
