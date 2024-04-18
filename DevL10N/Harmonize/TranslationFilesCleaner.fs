module DevL10N.Harmonize.TranslationFilesCleaner

open System.Reflection.Emit

open HarmonyLib
open Verse

open DevL10N.Lib


[<HarmonyPatch(typeof<TranslationFilesCleaner>, "CleanupTranslationFiles")>]
module CleanupTranslationFiles =
  type private IMarker =
    interface
    end

  let private empty (s: ModMetaData seq) = s |> Seq.filter (fun x -> x.IsCoreMod)

  // nullify official mod checking
  let Transpiler (instructions: CodeInstruction seq) =
    let insts = List.ofSeq instructions
    
    let first, rest =
      insts
      |> List.findIndex (fun inst -> inst.opcode = OpCodes.Stloc_1)
      |> (+) 1
      |> splitFlipped insts

    seq {
      yield! first
      yield CodeInstruction(OpCodes.Ldloc_1)
      yield CodeInstruction(OpCodes.Call, AccessTools.Method(typeof<IMarker>.DeclaringType, "empty"))
      yield CodeInstruction (OpCodes.Stloc_1)
      yield! rest
    }
