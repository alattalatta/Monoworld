module DevL10N.Harmonize.DebugToolsSpawning

open HarmonyLib
open Verse

open DevL10N.Harmonize


[<HarmonyPatch(typeof<DebugToolsSpawning>, "SpawnPawn")>]
module SpawnPawn =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugToolsSpawning>, "SpawnPawnWithLifestage")>]
module SpawnPawnWithLifestage =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugToolsSpawning>, "SpawnAtDevelopmentalStages")>]
module SpawnAtDevelopmentalStages =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugToolsSpawning>, "TryPlaceNearThingWithStyle")>]
module TryPlaceNearThingWithStyle =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugToolsSpawning>, "SpawnWeapon")>]
module SpawnWeapon =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugToolsSpawning>, "SpawnApparel")>]
module SpawnApparel =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugToolsSpawning>, "CreateMealWithSpecifics")>]
module CreateMealWithSpecifics =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugToolsSpawning>, "SpawnSite")>]
module SpawnSite =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugToolsSpawning>, "SpawnSiteWithPoints")>]
module SpawnSiteWithPoints =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugToolsSpawning>, "SpawnWorldObject")>]
module SpawnWorldObject =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugToolsSpawning>, "WaterEmergePawn")>]
module WaterEmergePawn =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions
