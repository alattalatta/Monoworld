module DevL10N.Harmonize.DebugTools_Health

open System.Reflection.Emit

open HarmonyLib
open Poet.Lib
open Verse


module private Base =
  type IMarker =
    interface
    end

  let private makeLabel (def: Def) =
    sprintf "%s{%s}" (def.label.CapitalizeFirst()) def.defName

  let transpile (instructions: CodeInstruction seq) =
    let insts = List.ofSeq instructions

    List.tryFindIndex
      (fun (inst: CodeInstruction) ->
        inst.opcode = OpCodes.Callvirt
        && inst.operand = AccessTools.PropertyGetter(typeof<Def>, "LabelCap"))
      insts
    |> Option.map (fun index ->
      let before, body = List.splitAt index insts

      // remove LabelCap + (implicit) TaggedString -> String
      let _, after = List.splitAt 2 body

      seq {
        yield! before
        yield CodeInstruction(OpCodes.Call, AccessTools.Method(typeof<IMarker>.DeclaringType, nameof (makeLabel)))
        yield! after
      })
    |> Option.defaultWith (fun () ->
      warn "[Dev In Your Language] Failed to patch DebugTools_Health"
      instructions)


[<HarmonyPatch(typeof<DebugTools_Health>, "AddHediff")>]
module AddHediff =
  let Transpiler (instructions: CodeInstruction seq) = Base.transpile instructions


[<HarmonyPatch(typeof<DebugTools_Health>, "Options_AddHediff")>]
module Options_AddHediff =
  let Transpiler (instructions: CodeInstruction seq) = Base.transpile instructions
