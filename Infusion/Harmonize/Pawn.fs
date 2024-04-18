module Infusion.Harmonize.Pawn

open HarmonyLib
open Poet.Lyric
open RimWorld
open Verse

open Infusion


[<HarmonyPatch(typeof<Pawn>, "Notify_Downed")>]
module Notify_Downed =
  let Postfix (__instance: Pawn) =
    Pawn.getApparels __instance
    |> Option.iter (fun apparels ->
      let apparelWorkerSets =
        apparels
        |> List.choose (fun apparel ->
          Comp.ofThing<CompInfusion> apparel
          |> Option.map (fun comp -> (apparel, comp.OnHits))
          |> Option.filter (fun (_, onHits) -> onHits.Length > 0))

      do
        Lib.runUntilFalseFrom
          0
          ((fun ((apparel, onHits): (Apparel * OnHitWorker list)) ->
            Lib.runUntilFalseFrom 0 ((fun (onHit: OnHitWorker) -> onHit.WearerDowned __instance apparel), onHits)),
           apparelWorkerSets)
        |> ignore)
