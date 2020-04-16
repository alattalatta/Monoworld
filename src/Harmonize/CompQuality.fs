module Infusion.Harmonize.CompQuality

open HarmonyLib
open RimWorld
open Verse

open Infusion
open Lib
open StatMod
open VerseInterop
open UnityEngine

[<HarmonyPatch(typeof<CompQuality>, "SetQuality")>]
module SetQuality =
    // 2, 1, 1, 1, 2, 3, 4
    let private maxInfusionsFor (quality: QualityCategory) = max 1 (abs (int quality - 2))

    /// Picks elligible `InfusionDef` for the `Thing`.
    let private pickInfusions (quality: QualityCategory) (parent: ThingWithComps) =
        // requirement fields
        let checkAllowance (infDef: InfusionDef) =
            if parent.def.IsApparel then infDef.requirements.allowance.apparel
            elif parent.def.IsMeleeWeapon then infDef.requirements.allowance.melee
            elif parent.def.IsRangedWeapon then infDef.requirements.allowance.ranged
            else false

        let checkTechLevel (infDef: InfusionDef) = infDef.requirements.techLevel |> Seq.contains parent.def.techLevel
        let checkQuality (infDef: InfusionDef) = (infDef.ChanceFor quality) > 0.0f

        // chance
        let checkChance (infDef: InfusionDef) =
            let chance = infDef.ChanceFor(quality)
            Rand.Chance chance

        DefDatabase<InfusionDef>.AllDefs
        |> Seq.filter (checkAllowance <&> checkTechLevel <&> checkQuality)
        |> Seq.map (fun infDef -> (infDef, (infDef.WeightFor quality * 1.2f) + Rand.Value)) // weighted, duh
        |> Seq.sortByDescending snd
        |> Seq.truncate (maxInfusionsFor quality)
        |> Seq.map fst
        |> Seq.filter checkChance
        |> List.ofSeq // need to "finalize" the random sort
        |> List.sortBy (fun infDef -> infDef.tier)

    /// Applies infusions to the `Thing` based on its quality.
    let Prefix(__instance: CompQuality, q: QualityCategory, source: ArtGenerationContext) =
        compOfThing<Comp.Infusion> __instance.parent
        |> Option.iter (fun compInfusion -> do compInfusion.InfusionsOrdered <- pickInfusions q __instance.parent)

    /// Resets `HitPoints` to its own `MaxHitPoints`, reflecting changes from infusions
    let Postfix(__instance: CompQuality) =
        // All hit points of a pawn's apparels are determined *after* SetQuality() call,
        // see: PawnGenerator.PostProcessGeneratedGear()
        // We can blindly reset any Thing's HitPoints to its MaxHitPoints.
        do __instance.parent.HitPoints <- __instance.parent.MaxHitPoints
