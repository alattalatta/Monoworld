module DevL10N.Harmonize.Listing_Standard

open System.Reflection.Emit

open LudeonTK
open HarmonyLib
open Verse

open DevL10N.Lib


[<HarmonyPatch(typeof<Dialog_Debug>, "DoButton")>]
module DoButton =
  type private IMarker =
    interface
    end

  // e.g. __translated__Destroy{Destroy} => __translated__Destroy
  let private makeLabel (label: string) = if label.NullOrEmpty() then "" else (label.Split('{')) |> Seq.head

  // hides the original label
  let Transpiler (instructions: CodeInstruction seq) =
    let insts = List.ofSeq instructions

    let first, others =
      insts
      |> List.findIndex (fun inst -> inst.opcode = OpCodes.Stloc_0)
      |> splitFlipped insts

    //            ldarg.1
    //            callvirt instance string ...
    // (*first*)  stloc.0
    //            makeLabel
    // (*others*) ...
    seq {
      yield! first
      yield CodeInstruction(OpCodes.Call, AccessTools.Method(typeof<IMarker>.DeclaringType, "makeLabel"))
      yield! others
    }
