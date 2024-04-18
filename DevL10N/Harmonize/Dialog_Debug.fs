module DevL10N.Harmonize.Listing_Standard

open LudeonTK
open HarmonyLib


[<HarmonyPatch(typeof<Dialog_Debug>, "DoButton")>]
module DoButton =
  let Transpiler (instructions: CodeInstruction seq) =
    instructions |> Seq.map (fun inst ->
    if inst.operand = 22f then
      new CodeInstruction(inst.opcode, 22f)
    else
      inst
  )
  

[<HarmonyPatch(typeof<Dialog_Debug>, "ButtonDebugPinnable")>]
module ButtonDebugPinnable =
  let Transpiler (instructions: CodeInstruction seq) =
    instructions |> Seq.map (fun inst ->
    if inst.operand = 22f then
      new CodeInstruction(inst.opcode, 22f)
    else
      inst
  )