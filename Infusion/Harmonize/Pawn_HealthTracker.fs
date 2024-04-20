module Infusion.Harmonize.Pawn

open System

open HarmonyLib
open Poet.Lyric
open RimWorld
open Verse

open Infusion


// Can't use Notify_Downed, need dinfo
[<HarmonyPatch(typeof<Pawn_HealthTracker>, "MakeDowned")>]
module MakeDowned =
  let pawnFi = AccessTools.Field(typeof<Pawn_HealthTracker>, "pawn")

  let Postfix (dinfo: Nullable<DamageInfo>, hediff: Hediff, __instance: Pawn_HealthTracker) =
    // only for downs from damages
    if dinfo.HasValue then
      let pawn = pawnFi.GetValue(__instance) :?> Pawn

      Pawn.getApparels (pawn)
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
              Lib.runUntilFalseFrom 0 ((fun (onHit: OnHitWorker) -> onHit.WearerDowned pawn apparel), onHits)),
             apparelWorkerSets)
          |> ignore)
