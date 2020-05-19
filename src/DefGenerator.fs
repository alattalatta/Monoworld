module Infusion.DefGenerator

open RimWorld
open Verse

open VerseInterop

let makeInfuserDef (tier: TierDef) =
    let comps: CompProperties list =
        [ CompInfuserProperties(1, tier)
          CompProperties_Forbiddable() ]

    let stats =
        [ StatModifier(stat = StatDefOf.MaxHitPoints, value = 80.0f)
          StatModifier(stat = StatDefOf.Mass, value = 0.5f)
          StatModifier(stat = StatDefOf.MarketValue, value = tier.infuserValue)
          StatModifier(stat = StatDefOf.SellPriceFactor, value = 0.2f) ]

    ThingDef
        (defName = "Infusion_Infuser_" + tier.defName,
         label = string (translate1 "Infusion.Infuser.Label" tier.label),
         description = string (translate "Infusion.Infuser.Description"),
         category = ThingCategory.Item,
         thingCategories =
             new System.Collections.Generic.List<ThingCategoryDef>([ ThingCategoryDef.Named("Infusion_Infusers") ]),
         selectable = true,
         thingClass = typeof<Infuser>,
         comps = new System.Collections.Generic.List<CompProperties>(comps),
         graphicData = GraphicData(texPath = "Things/Infuser", graphicClass = typeof<Graphic_Single>),
         statBases = new System.Collections.Generic.List<StatModifier>(stats),
         techLevel = TechLevel.Ultra,
         alwaysHaulable = true,
         rotatable = false,
         pathCost = 15,
         tradeTags = new System.Collections.Generic.List<string>([ "ExoticMisc"; "Infusion_Infuser" ]),
         thingSetMakerTags = new System.Collections.Generic.List<string>([ "RewardStandardHighFreq" ]))

let makeInfuserDefs () =
    DefDatabase<TierDef>.AllDefs
    |> Seq.filter (fun td -> td.infusable)
    |> Seq.map makeInfuserDef
