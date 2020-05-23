module Infusion.Harmonize.StatWorker

open System.Collections.Generic
open System.Reflection.Emit
open System.Text

open HarmonyLib
open RimWorld
open Verse

open Infusion
open Lib
open StatMod
open VerseTools
open VerseInterop
open System.Reflection


module StatWorker =
    let apparelsAndEquipments (pawn: Pawn) =
        let fromApparels =
            match apparelsOfPawn pawn with
            | Some apparels -> apparels |> List.map upcastToThing
            | None -> List.empty

        let fromEquipments =
            match equipmentsOfPawn pawn with
            | Some equipments -> equipments |> List.map upcastToThing
            | None -> List.empty

        List.concat [ fromApparels; fromEquipments ]

    let tryCastToPawn (thing: Thing) =
        match thing with
        | :? Pawn as p -> Some p
        | _ -> None


[<HarmonyPatch(typeof<StatWorker>, "RelevantGear")>]
module RelevantGear =
    /// Adds hyperlink entries in the pawn's inspection window.
    /// Why not a "GearAffectsStat"? Because it uses a ThingDef, not a Thing.
    let Postfix (returned: Thing seq, pawn: Pawn, stat: StatDef) =
        if Set.contains stat.category.defName pawnStatCategories then
            let map = Dictionary<string, Thing>()
            returned
            |> Seq.iter (fun thing -> map.Add(thing.ThingID, thing))

            StatWorker.apparelsAndEquipments pawn
            |> List.choose (fun t ->
                compOfThing<CompInfusion> t
                |> Option.filter (fun comp -> comp.HasInfusionForStat stat))
            |> List.iter (fun t -> map.SetOrAdd(t.parent.ThingID, t.parent))

            seq (map.Values)

        else
            returned


[<HarmonyPatch(typeof<StatWorker>, "InfoTextLineFromGear")>]
module InfoTextLineFromGear =
    let Prefix (gear: Thing, stat: StatDef, __result: outref<string>) =
        let baseMod =
            StatMod((gear.def.equippedStatOffsets.GetStatOffsetFromList stat), 0.0f)

        let fromInfusion =
            compOfThing<CompInfusion> gear
            |> Option.map (fun c -> c.GetModForStat stat)
            |> Option.defaultValue (StatMod.empty)

        do __result <-
            "    "
            + gear.LabelCap
            + ": "
            + (stringForStat stat (baseMod + fromInfusion))

        false


[<HarmonyPatch(typeof<StatWorker>, "GetExplanationUnfinalized")>]
module GetExplanationUnfinalized =
    type private Marker =
        interface
        end

    let gearOrInfusionAffectsStat (gear: Thing, stat: StatDef) =
        if gear.def.equippedStatOffsets.GetStatOffsetFromList(stat) > 0.0f then
            true
        else
            compOfThing<CompInfusion> gear
            |> Option.map (fun c -> c.HasInfusionForStat stat)
            |> Option.defaultValue false

    /// Replace gear stat predicate to use customized version.
    let Transpiler (instructions: CodeInstruction seq) =
        let insts = List.ofSeq instructions

        let statFI = AccessTools.Field(typeof<Thing>, "def")

        let gearAffectsStatMI =
            AccessTools.Method(typeof<StatWorker>, "GearAffectsStat")

        let gearOrInfusionAffectsStatMI =
            AccessTools.Method(typeof<Marker>.DeclaringType, "gearOrInfusionAffectsStat")

        for i in 0 .. (List.length insts) - 1 do
            let inst = insts.[i]
            if inst.opcode = OpCodes.Ldfld
               && (inst.operand :?> FieldInfo) = statFI then
                do inst.opcode <- OpCodes.Nop
            elif inst.opcode = OpCodes.Call
                 && (inst.operand :?> MethodInfo) = gearAffectsStatMI then
                do inst.operand <- gearOrInfusionAffectsStatMI

        Seq.ofList insts


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

    let relevantMods (stat: StatDef) (pawn: Pawn) =
        StatWorker.apparelsAndEquipments pawn
        |> List.choose (fun t ->
            compOfThing<CompInfusion> t
            |> Option.map (fun comp -> comp.GetModForStat stat))
        |> List.fold (+) StatMod.empty

    let getInfusionMultiplier (req: StatRequest, stat: StatDef) =
        if Set.contains stat.category.defName pawnStatCategories then
            StatWorker.tryCastToPawn req.Thing
            |> Option.map (relevantMods stat)
            |> Option.map (fun m -> m.multiplier + 1.0f)
            |> Option.defaultValue 1.0f
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
