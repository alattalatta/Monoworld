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
    Poet.Lyric.Console.log "%A (%A)" __instance.parent.def.defName __instance.parent.ThingID
    __instance.parent.AllComps |> Seq.iter (fun c ->
      Poet.Lyric.Console.log "\t%A" c
    )

    do
      Comp.ofThing<CompInfusion> __instance.parent
      |> Option.iter (fun compInfusion ->
        do compInfusion.Quality <- __instance.Quality
        do compInfusion.Infusions <- CompInfusion.pickInfusions __instance.Quality compInfusion
        Poet.Lyric.Console.log "\t%A (%A) %A -> %A" __instance.parent.def.defName __instance.parent.ThingID compInfusion.Quality compInfusion.Infusions
        // All hit points of a pawn's apparels are determined *after* SetQuality() call,
        // see: PawnGenerator.PostProcessGeneratedGear()
        // We can blindly reset any Thing's HitPoints to its MaxHitPoints.
        do Comp.getParent __instance |> resetHP)
