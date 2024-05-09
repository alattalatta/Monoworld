module Infusion.Harmonize.StatWorker

open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

open HarmonyLib
open Poet.Lyric
open RimWorld
open Verse

open Infusion
open StatMod
open VerseTools


let private apparelsAndEquipments (pawn: Pawn) =
  let fromApparels =
    match Pawn.getApparels pawn with
    | Some apparels -> apparels |> List.map upcastToThing
    | None -> List.empty

  let fromEquipments =
    match Pawn.getEquipments pawn with
    | Some equipments -> equipments |> List.map upcastToThing
    | None -> List.empty

  List.concat [ fromApparels
                fromEquipments ]


type StatEligibility = PawnStat | ArmorStat | Ineligible
let private statsEligibilityMap = Dictionary<StatDef, StatEligibility>()


let populateStatsEligibilityMap () =
  DefDatabase<StatDef>.AllDefs
  |> Seq.iter (fun stat ->
    let statEligibility =
      match stat.category with
      | null -> Ineligible
      | _ as s ->
        if Set.contains s.defName pawnStatCategories then PawnStat
        elif Set.contains s.defName armorStats then ArmorStat
        else Ineligible
    statsEligibilityMap.SetOrAdd(stat, statEligibility)
  )


[<HarmonyPatch(typeof<StatWorker>, "RelevantGear")>]
module RelevantGear =
  /// Adds hyperlink entries in the pawn's inspection window.
  /// Why not the "GearAffectsStat"? Because it uses a ThingDef, not a Thing.
  let Postfix (returned: Thing seq, pawn: Pawn, stat: StatDef) =
    // Just skip animals
    if not pawn.def.race.Humanlike then
      returned
    else
      let isPawnStat = Set.contains stat.category.defName pawnStatCategories

      let isArmorStat = Set.contains stat.defName armorStats

      if isPawnStat || isArmorStat then
        let map = Dictionary<string, Thing>()

        for thing in returned do
          map.Add(thing.ThingID, thing)

        let thingsToConsider =
          if isPawnStat then
            apparelsAndEquipments pawn
          else
            // armor stats = only weapons, no need to calculate
            match Pawn.getEquipments pawn with
            | Some equipments -> equipments |> List.map upcastToThing
            | None -> List.empty

        thingsToConsider
        |> List.choose (fun t ->
          Comp.ofThing<CompInfusion> t
          |> Option.filter (fun comp -> comp.HasInfusionForStat stat))
        |> List.iter (fun t -> map.SetOrAdd(t.parent.ThingID, t.parent))

        seq (map.Values)

      else
        returned


[<HarmonyPatch(typeof<StatWorker>, "StatOffsetFromGear")>]
module StatOffsetFromGear =
  /// Adds infusions to Core stat calculation.
  /// Note that we can only use `StatMod#offset` because it is "stat _offset_ from gear."
  let Postfix (returned: float32, gear: Thing, stat: StatDef) =
    let statType = statsEligibilityMap.TryGetValue(stat, Ineligible)

    if statType <> Ineligible then
      match Comp.ofThing<CompInfusion> gear with
      | Some compInf ->
        // for general stats, considers both weapons and apparels into account
        // for armor stats, only checks weapons
        if statType = PawnStat || (statType = ArmorStat && gear.def.IsWeapon) then
          (compInf.GetModForStat stat).offset + returned
        else
          returned
      | None -> returned
    else
      returned



[<HarmonyPatch(typeof<StatWorker>, "InfoTextLineFromGear")>]
module InfoTextLineFromGear =
  let Prefix (gear: Thing, stat: StatDef, __result: outref<string>) =
    let baseMod =
      StatMod((gear.def.equippedStatOffsets.GetStatOffsetFromList stat), 0.0f)

    let fromInfusion =
      Comp.ofThing<CompInfusion> gear
      |> Option.map (fun c -> c.GetModForStat stat)
      |> Option.defaultValue (StatMod.empty)

    __result <-
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
      Comp.ofThing<CompInfusion> gear
      |> Option.map (fun c -> c.HasInfusionForStat stat)
      |> Option.defaultValue false

  /// Replace gear stat predicate to use customized version.
  let Transpiler (instructions: CodeInstruction seq) =
    let insts = List.ofSeq instructions

    let statFI = AccessTools.Field(typeof<Thing>, "def")

    let gearAffectsStatMI = AccessTools.Method(typeof<StatWorker>, "GearAffectsStat")

    let gearOrInfusionAffectsStatMI =
      AccessTools.Method(typeof<Marker>.DeclaringType, "gearOrInfusionAffectsStat")

    for i in 0 .. (List.length insts) - 1 do
      let inst = insts.[i]

      if inst.opcode = OpCodes.Ldfld
         && (inst.operand :?> FieldInfo) = statFI then
        inst.opcode <- OpCodes.Nop
      elif inst.opcode = OpCodes.Call
           && (inst.operand :?> MethodInfo) = gearAffectsStatMI then
        inst.operand <- gearOrInfusionAffectsStatMI

    Seq.ofList insts
