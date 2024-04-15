module DevL10N.Harmonize.Dialog_DebugActionsMenu

open System.Reflection
open System.Reflection.Emit

open HarmonyLib
open LudeonTK
open Verse

open DevL10N.Lib


let supportedAssemblies = [ "Assembly-CSharp"; "DevL10N" ]

/// If the method is from Assembly-CSharp, call TranslateSimple().
/// if not, apply the default behavior.
let translatableFromMethodInfo (attrName: string) (mi: MethodInfo) =
  if List.contains (mi.DeclaringType.Assembly.GetName().Name) supportedAssemblies then
    let originalLabel =
      if attrName.NullOrEmpty() then
        GenText.SplitCamelCase mi.Name
      else
        attrName

    sprintf "%s{%s}" (("DebugAction_" + mi.Name).TranslateSimple()) originalLabel

  else if attrName.NullOrEmpty() then
    GenText.SplitCamelCase mi.Name

  else
    attrName

[<HarmonyPatch(typeof<DebugTabMenu_Actions>, "GenerateCacheForMethod")>]
module GenerateCacheForMethod =
  type private IMarker =
    interface
    end

  let private makeLabel (mi: MethodInfo, attr: DebugActionAttribute) = translatableFromMethodInfo attr.name mi

#if DEBUG
  let Prefix (method: MethodInfo, attribute: DebugActionAttribute) =
    Log.Message(
      taggify
        "DebugAction"
        method.Name
        ((makeLabel (method, attribute)).Split('{')
         |> Seq.head)
    )

    true
#endif

  // change
  //   string text = (string.IsNullOrEmpty(attribute.name) ? GenText.SplitCamelCase(method.Name) : attribute.name);
  // into
  //   string text = makeLabel(method, attribute);
  let Transpiler (instructions: CodeInstruction seq) =
    let insts = List.ofSeq instructions

    // skip all instructions to the first ldarg_1, which is
    // if (attribute.actionType == DebugActionType.ToolMap || attribute.actionType == DebugActionType.ToolMapForPawns ...
    let _, rest =
      insts
      |> List.findIndex (fun inst -> inst.opcode = OpCodes.Ldarg_1)
      |> (+) -1
      |> splitFlipped insts

    seq {
      yield CodeInstruction(OpCodes.Ldarg_1)
      yield CodeInstruction(OpCodes.Ldarg_2)
      yield CodeInstruction(OpCodes.Call, AccessTools.Method(typeof<IMarker>.DeclaringType, "makeLabel"))
      yield! rest
    }
