module Infusion.ResourceBank

open Poet.Lyric.Translation
open UnityEngine
open Verse


[<StaticConstructorOnStartup>]
type Textures() =
  static let flame = ContentFinder<Texture2D>.Get ("Things/Special/Fire/FireA")

  static member Flame = flame


module Strings =
  module Matchers =
    let apparel = translate "Infusion.Matchers.Apparel"
    let melee = translate "Infusion.Matchers.Melee"
    let ranged = translate "Infusion.Matchers.Ranged"

    let notUtility = translate "Infusion.Matchers.NotUtility"

    let negate str =
      translate1 "Infusion.Matchers.Negate" str

    let shieldBelt = translate "Infusion.Matchers.ShieldBelt"


  module Gizmo =
    let label = translate "Infusion.EffectGizmo"

    let desc = translate "Infusion.EffectGizmo.Description"


  module ITab =
    let hint = translate "Infusion.ITab.Hint"

    let noSlot = translate "Infusion.ITab.NoSlot"

    // marks
    let markExtraction extractionChance =
      translate1 "Infusion.ITab.MarkForExtraction" extractionChance

    let markRemoval extractionChance =
      translate1 "Infusion.ITab.MarkForRemoval" extractionChance

    let unmark = translate "Infusion.ITab.Unmark"

    // applying infusers
    let applyInfuser = translate "Infusion.ITab.ApplyInfuser"

    let cancelInfuser = translate "Infusion.ITab.CancelInfuser"

    let applyInfuserDesc = translate "Infusion.ITab.ApplyInfuser.Description"

    let cantApplySlotsFull = translate "Infusion.ITab.ApplyInfuser.SlotsFull"

    let cantApplyNoSuitable = translate "Infusion.ITab.ApplyInfuser.NoSuitableInfuser"
