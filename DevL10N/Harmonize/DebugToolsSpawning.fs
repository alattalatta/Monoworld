module DevL10N.Harmonize.DebugThingPlaceHelper

open HarmonyLib
open Verse

open DevL10N.Harmonize


[<HarmonyPatch(typeof<DebugThingPlaceHelper>, "SpawnOptions")>]
module SpawnOptions =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugThingPlaceHelper>, "TryAbandonOptionsForStackCount")>]
module TryAbandonOptionsForStackCount =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugThingPlaceHelper>, "TryPlaceOptionsForBaseMarketValue")>]
module TryPlaceOptionsForBaseMarketValue =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugThingPlaceHelper>, "TryPlaceOptionsForStackCount")>]
module TryPlaceOptionsForStackCount =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions


[<HarmonyPatch(typeof<DebugThingPlaceHelper>, "TryPlaceOptionsUnminified")>]
module TryPlaceOptionsUnminified =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions
