module Infusion.Harmonize.CompQuality

open HarmonyLib
open RimWorld

open Infusion
open VerseInterop
open VerseTools

[<HarmonyPatch(typeof<CompQuality>, "SetQuality")>]
module SetQuality =
    /// Applies infusions to a `Thing` based on its quality.
    // Note: The name "q" can't be changed
    let Prefix (__instance: CompQuality, q: QualityCategory, source: ArtGenerationContext) =
        do compOfThing<Comp.Infusion> __instance.parent
           |> Option.iter (fun compInfusion ->
               do compInfusion.Quality <- q
               do compInfusion.Infusions <- Comp.pickInfusions q __instance.parent)

    /// Resets a `Thing`'s `HitPoints` to its own `MaxHitPoints` to reflect HP changes caused by infusions.
    let Postfix (__instance: CompQuality) =
        // All hit points of a pawn's apparels are determined *after* SetQuality() call,
        // see: PawnGenerator.PostProcessGeneratedGear()
        // We can blindly reset any Thing's HitPoints to its MaxHitPoints.
        do parentOfComp __instance |> resetHP
