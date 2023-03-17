module Infusion.Harmonize.CompQuality

open HarmonyLib
open Poet.Lib
open Poet.Lyric
open RimWorld

open Infusion
open VerseTools


[<HarmonyPatch(typeof<CompQuality>, "SetQuality")>]
// main flow for infusing
module SetQuality =
  // Note: The name "q" can't be changed
  let Prefix (__instance: CompQuality, q: QualityCategory, source: ArtGenerationContext) =
    do
      Comp.ofThing<CompInfusion> __instance.parent
      |> Option.iter (fun compInfusion ->
        do compInfusion.Quality <- q
        do compInfusion.Infusions <- CompInfusion.pickInfusions q compInfusion)

  let Postfix (__instance: CompQuality) =
    // do StatDefOf.MaxHitPoints.Worker.TryClearCache()

    // All hit points of a pawn's apparels are determined *after* SetQuality() call,
    // see: PawnGenerator.PostProcessGeneratedGear()
    // We can blindly reset any Thing's HitPoints to its MaxHitPoints.
    do Comp.getParent __instance |> resetHP
