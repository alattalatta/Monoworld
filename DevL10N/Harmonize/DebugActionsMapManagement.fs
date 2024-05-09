module DevL10N.Harmonize.DebugActionsMapManagement

open HarmonyLib
open Verse

open DevL10N.Harmonize


[<HarmonyPatch(typeof<DebugActionsMapManagement>, "SetTerrainRect")>]
module SetTerrainRect =
  let Transpiler (instructions: CodeInstruction seq) = ActionOptionHelper.transpile instructions
