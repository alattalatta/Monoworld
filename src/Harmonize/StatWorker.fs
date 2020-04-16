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
        let moddedResult = List.ofSeq returned

        let infusedApparels =
            match apparelsOfPawn pawn with
            | Some apparels ->
                apparels
                |> List.filter (fun apparel ->
                    match compOfThing<Comp.Infusion> apparel with
                    | Some compInf -> compInf.HasInfusionForStat stat
                    | None -> false)
            | None -> List.empty
            |> List.map (fun apparel -> apparel :> Thing)

        let infusedEquipments =
            match equipmentsOfPawn pawn with
            | Some equipments ->
                equipments
                |> List.filter (fun equipment ->
                    match compOfThing<Comp.Infusion> equipment with
                    | Some compInf -> compInf.HasInfusionForStat stat
                    | None -> false)
            | None -> List.empty
            |> List.map (fun equipment -> equipment :> Thing)

        seq {
            yield! moddedResult
            yield! infusedApparels
            yield! infusedEquipments
        }
        |> Seq.distinctBy (fun thing -> thing.thingIDNumber)

[<HarmonyPatch(typeof<StatWorker>, "StatOffsetFromGear")>]
module StatOffsetFromGear =
    /// Adds infusions to Core stat calculation.
    /// Note that we can only use `StatMod#offset` because it is "stat _offset_ from gear."
    let Postfix(returned: float32, gear: Thing, stat: StatDef) =
        let baseValue = gear.def.equippedStatOffsets.GetStatOffsetFromList stat

        match compOfThing<Comp.Infusion> gear with
        | Some compInf -> (compInf.GetAllStatModsForStat stat |> List.fold (+) StatMod.empty).offset + baseValue
        | None -> returned
