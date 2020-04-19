module Infusion.Harmonize.StatWorker

open System.Collections.Generic

open HarmonyLib
open RimWorld
open Verse

open Infusion
open Infusion.StatMod
open Infusion.VerseInterop

[<HarmonyPatch(typeof<StatWorker>, "RelevantGear")>]
module RelevantGear =
    /// Adds hyperlink entries in the pawn's inspection window.
    let Postfix(returned: IEnumerable<Thing>, pawn: Pawn, stat: StatDef) =
        let map = Dictionary<string, Thing>()
        returned |> Seq.iter (fun thing -> map.Add(thing.ThingID, thing))

        match apparelsOfPawn pawn with
        | Some apparels ->
            apparels
            |> Seq.filter (fun apparel ->
                match compOfThing<Comp.Infusion> apparel with
                | Some compInf -> compInf.HasInfusionForStat stat
                | None -> false)
        | None -> Seq.empty
        |> Seq.iter (fun apparel -> map.SetOrAdd(apparel.ThingID, apparel))

        match equipmentsOfPawn pawn with
        | Some equipments ->
            equipments
            |> Seq.filter (fun equipment ->
                match compOfThing<Comp.Infusion> equipment with
                | Some compInf -> compInf.HasInfusionForStat stat
                | None -> false)
        | None -> Seq.empty
        |> Seq.iter (fun equipment -> map.SetOrAdd(equipment.ThingID, equipment))

        seq (map.Values)

[<HarmonyPatch(typeof<StatWorker>, "StatOffsetFromGear")>]
module StatOffsetFromGear =
    /// Adds infusions to Core stat calculation.
    /// Note that we can only use `StatMod#offset` because it is "stat _offset_ from gear."
    let Postfix(returned: float32, gear: Thing, stat: StatDef) =
        let baseValue = gear.def.equippedStatOffsets.GetStatOffsetFromList stat

        match compOfThing<Comp.Infusion> gear with
        | Some compInf ->
            (compInf
             |> Comp.allModsForStat stat
             |> List.fold (+) StatMod.empty).offset
            + baseValue
        | None -> returned
