namespace Infusion

open System

open UnityEngine
open Verse

open DefFields

type TierDef =
    inherit HashEqualDef

    val mutable color: Color

    val mutable chances: QualityMap
    val mutable weights: QualityMap

    val mutable maxCount: int

    // used for sorting infusions, higher being higher
    val mutable priority: int

    // market value of the generated infuser
    val mutable infuserValue: float32
    // extraction success chance
    val mutable extractionChance: float32

    val mutable infuser: ThingDef

    new() =
        { inherit HashEqualDef()
          color = Color.white
          chances = QualityMap()
          weights = QualityMap()
          maxCount = -1
          priority = 0
          infuserValue = 100.0f
          extractionChance = 1.0f

          infuser = null }

    override this.Equals ob = base.Equals ob

    override this.GetHashCode() = base.GetHashCode()

    interface IComparable with
        member this.CompareTo(ob) =
            match ob with
            | :? TierDef as def -> this.defName.CompareTo def.defName
            | _ -> 0


module TierDef =
    let empty = TierDef()

    let isEmpty (tier: TierDef) = tier.defName = "UnnamedDef"
