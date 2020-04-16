module Infusion.DefTool

open RimWorld
open UnityEngine
open Verse

open DefFields
open VerseInterop

// Because StatCategoryDef doesn't implement IComparable,
// defs can't be used directly for Sets.
let pawnStatCategories =
    Set.ofList
        [ StatCategoryDefOf.BasicsPawn.defName
          StatCategoryDefOf.BasicsPawnImportant.defName
          StatCategoryDefOf.PawnCombat.defName
          StatCategoryDefOf.PawnMisc.defName
          StatCategoryDefOf.PawnSocial.defName
          StatCategoryDefOf.PawnWork.defName ]

let apparelOrWeapon (def: ThingDef) =
    ThingCategoryDefOf.Apparel.ContainedInThisOrDescendant def
    || ThingCategoryDefOf.Weapons.ContainedInThisOrDescendant def

let getLabelOfTier (tier: Tier) =
    match tier with
    | Tier.Awful -> translate "Infused.Awful"
    | Tier.Poor -> translate "Infused.Poor"
    | Tier.Uncommon -> translate "Infused.Uncommon"
    | Tier.Rare -> translate "Infused.Rare"
    | Tier.Epic -> translate "Infused.Epic"
    | Tier.Legendary -> translate "Infused.Legendary"
    | Tier.Artifact -> translate "Infused.Artifact"
    | _ -> ""

let tierToColor (tier: Tier) =
    match tier with
    | Tier.Awful
    | Tier.Poor -> Color(0.61f, 0.61f, 0.61f)
    | Tier.Uncommon -> Color(0.11f, 1.0f, 0.0f)
    | Tier.Rare -> Color(0.0f, 0.43f, 0.86f)
    | Tier.Epic -> Color(0.57f, 0.27f, 1.0f)
    | Tier.Legendary -> Color(1.0f, 0.5f, 0.0f)
    | Tier.Artifact -> Color(0.89f, 0.8f, 0.5f)
    | _ -> Color.white
