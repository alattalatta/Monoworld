module Infusion.DefGenerator

open Poet.Lyric.Translation
open RimWorld
open Verse


let makeInfuserDef (tier: TierDef) =
  let comps: CompProperties list =
    [ CompInfuserProperties(tier)
      CompProperties_Forbiddable() ]

  let stats =
    [ StatModifier(stat = StatDefOf.MaxHitPoints, value = 80.0f)
      StatModifier(stat = StatDefOf.Mass, value = 0.5f)
      StatModifier(stat = StatDefOf.MarketValue, value = tier.infuserValue)
      StatModifier(stat = StatDefOf.SellPriceFactor, value = 0.2f) ]

  let infuser =
    ThingDef(
      alwaysHaulable = true,
      category = ThingCategory.Item,
      comps = new System.Collections.Generic.List<CompProperties>(comps),
      defName = "Infusion_Infuser_" + tier.defName,
      description = string (translate "Infusion.Infuser.Description"),
      drawGUIOverlay = true,
      graphicData = GraphicData(texPath = "Things/Infuser", graphicClass = typeof<Graphic_Single>),
      label = string (translate1 "Infusion.Infuser.Label" tier.label),
      pathCost = 15,
      rotatable = false,
      selectable = true,
      statBases = new System.Collections.Generic.List<StatModifier>(stats),
      techLevel = TechLevel.Ultra,
      thingCategories =
        new System.Collections.Generic.List<ThingCategoryDef>([ ThingCategoryDef.Named("Infusion_Infusers") ]),
      thingClass = typeof<Infuser>,
      tradeability = Tradeability.Buyable,
      tradeTags = new System.Collections.Generic.List<string>([ "Infusion_Infuser" ])
    )

  tier.infuser <- infuser

  infuser

let makeInfuserDefs () =
  DefDatabase<TierDef>.AllDefs
  |> Seq.map makeInfuserDef
