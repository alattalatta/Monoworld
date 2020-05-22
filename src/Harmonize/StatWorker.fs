module Infusion.Harmonize.StatWorker

open System.Collections.Generic
open System.Reflection.Emit

open HarmonyLib
open RimWorld
open Verse

open Infusion
open Lib
open VerseTools
open VerseInterop

[<HarmonyPatch(typeof<StatWorker>, "RelevantGear")>]
module RelevantGear =
    /// Adds hyperlink entries in the pawn's inspection window.
    let Postfix (returned: IEnumerable<Thing>, pawn: Pawn, stat: StatDef) =
        if Set.contains stat.category.defName pawnStatCategories then
            let map = Dictionary<string, Thing>()
            returned
            |> Seq.iter (fun thing -> map.Add(thing.ThingID, thing))

            match apparelsOfPawn pawn with
            | Some apparels ->
                apparels
                |> Seq.filter (fun apparel ->
                    match compOfThing<CompInfusion> apparel with
                    | Some compInf -> compInf.HasInfusionForStat stat
                    | None -> false)
            | None -> Seq.empty
            |> Seq.iter (fun apparel -> map.SetOrAdd(apparel.ThingID, apparel))

            match equipmentsOfPawn pawn with
            | Some equipments ->
                equipments
                |> Seq.filter (fun equipment ->
                    match compOfThing<CompInfusion> equipment with
                    | Some compInf -> compInf.HasInfusionForStat stat
                    | None -> false)
            | None -> Seq.empty
            |> Seq.iter (fun equipment -> map.SetOrAdd(equipment.ThingID, equipment))

            seq (map.Values)

        else
            returned


[<HarmonyPatch(typeof<StatWorker>, "StatOffsetFromGear")>]
module StatOffsetFromGear =
    /// Adds infusions to Core stat calculation.
    /// Note that we can only use `StatMod#offset` because it is "stat _offset_ from gear."
    let Postfix (returned: float32, gear: Thing, stat: StatDef) =
        if Set.contains stat.category.defName pawnStatCategories then
            match compOfThing<CompInfusion> gear with
            | Some compInf -> (compInf.GetModForStat stat).offset + returned
            | None -> returned
        else
            returned


[<HarmonyPatch(typeof<StatWorker>, "FinalizeValue")>]
module FinalizeValue =
    type private Marker =
        interface
        end

    let getInfusionMultiplier (req: StatRequest, stat: StatDef) =
        if Set.contains stat.category.defName pawnStatCategories then
            match compOfThing<CompInfusion> req.Thing with
            | Some compInf -> (compInf.GetModForStat stat).multiplier + 1.0f
            | None -> 1.0f
        else
            1.0f

    /// Applies infusions' stat multipliers **after** postprocessing curve takes place.
    let Transpiler (instructions: seq<CodeInstruction>) =
        let instLength = Seq.length instructions
        let retInst = Seq.last instructions // need to preserve labels on the ret

        let statFI =
            AccessTools.Field(typeof<StatWorker>, "stat")

        let getInfusionMultiplierMI =
            AccessTools.Method(typeof<Marker>.DeclaringType, "getInfusionMultiplier")

        // val *= getInfusionMultiplier(req, stat)
        let newInstructions =
            seq {
                yield CodeInstruction(OpCodes.Ldarg_2) // val ... a
                yield CodeInstruction(OpCodes.Ldarg_2) // val ... b
                yield CodeInstruction(OpCodes.Ldind_R4)
                yield CodeInstruction(OpCodes.Ldarg_1) // req
                yield CodeInstruction(OpCodes.Ldarg_0) // this
                yield CodeInstruction(OpCodes.Ldfld, statFI) // .stat
                yield CodeInstruction(OpCodes.Call, getInfusionMultiplierMI) // getInfusionMultiplier(req, stat) ... c
                yield CodeInstruction(OpCodes.Mul)
                yield CodeInstruction(OpCodes.Stind_R4) // a <- b * c
                yield retInst
            }

        seq {
            yield! instructions |> Seq.truncate (instLength - 1)
            yield! newInstructions
        }
