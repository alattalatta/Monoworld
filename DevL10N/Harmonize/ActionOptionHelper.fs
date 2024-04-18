module DevL10N.Harmonize.ActionOptionHelper

open System.Reflection.Emit

open HarmonyLib
open Verse


module private Base =
  type IMarker =
    interface
    end

  let private makeLabel (def: Def) =
    sprintf "%s{%s}" (def.label.CapitalizeFirst()) def.defName

// change
//   def.defName
// into
//   makeLabel def
let transpile (instructions: CodeInstruction seq) =
  instructions
  |> Seq.map (fun inst ->
    if inst.opcode = OpCodes.Ldfld
        && inst.operand = AccessTools.Field(typeof<Def>, "defName") then
      CodeInstruction(OpCodes.Call, AccessTools.Method(typeof<Base.IMarker>.DeclaringType, "makeLabel"))
    else
      inst)