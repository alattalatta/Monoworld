module DevL10N.Harmonize.Listing_Standard

open System.Reflection.Emit

open HarmonyLib
open Verse

open DevL10N.Lib


[<HarmonyPatch(typeof<Listing_Standard>, "ButtonDebug")>]
module ButtonDebug =
  type private IMarker =
    interface
    end

  // __translated__Destroy[Destroy] => __translated_Destroy
  let private makeLabel (label: string) = if label.NullOrEmpty() then "" else (label.Split('{')) |> Seq.head

  // hides the original label
  let Transpiler (instructions: CodeInstruction seq) =
    let insts = List.ofSeq instructions

    // instructions including the only ldarg_1
    let first, others =
      insts
      |> List.findIndex (fun inst -> inst.opcode = OpCodes.Ldarg_1)
      |> (+) 1
      |> splitFlipped insts

    seq {
      yield! first
      yield CodeInstruction(OpCodes.Call, AccessTools.Method(typeof<IMarker>.DeclaringType, "makeLabel"))
      yield! others
    }
