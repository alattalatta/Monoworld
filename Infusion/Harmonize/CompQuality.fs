module Infusion.Harmonize.CompQuality

open HarmonyLib
open Poet.Lyric
open RimWorld

open Infusion
open VerseTools


[<HarmonyPatch(typeof<CompQuality>, "SetQuality")>]
// main flow for infusing
module SetQuality =
  let Postfix (__instance: CompQuality) =
    Comp.ofThing<CompInfusion> __instance.parent
    |> Option.iter (fun compInfusion ->
      compInfusion.Quality <- __instance.Quality
      compInfusion.SetInfusions(CompInfusion.pickInfusions __instance.Quality compInfusion, false)
      // All hit points of a pawn's apparels are determined *after* SetQuality() call,
      // see: PawnGenerator.PostProcessGeneratedGear()
      // We can blindly reset any Thing's HitPoints to its MaxHitPoints.
      Comp.getParent __instance |> resetHP)
