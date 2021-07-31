module DevL10N.Harmonize.Dialog_DebugActionsMenu

open System.Reflection
open System.Reflection.Emit

open HarmonyLib
open Verse

open DevL10N.Lib


/// If the method is from Assembly-CSharp, call TranslateSimple().
/// if not, apply the default behavior.
let translatableFromMethodInfo (attrName: string) (mi: MethodInfo) =
    if mi.DeclaringType.Assembly.GetName().Name = "Assembly-CSharp" then
        let originalLabel =
            if attrName.NullOrEmpty() then
                GenText.SplitCamelCase mi.Name
            else
                attrName

        sprintf "%s[%s]" (("DebugAction_" + mi.Name).TranslateSimple()) originalLabel

    else if attrName.NullOrEmpty() then
        GenText.SplitCamelCase mi.Name

    else
        attrName

[<HarmonyPatch(typeof<Dialog_DebugActionsMenu>, "GenerateCacheForMethod")>]
module GenerateCacheForMethod =
    type private IMarker =
        interface
        end

    let private makeLabel (mi: MethodInfo, attr: DebugActionAttribute) = translatableFromMethodInfo attr.name mi

    // HOW LAZY AM I
    // [todo] Use compiler directives
    let __Prefix (method: MethodInfo, attribute: DebugActionAttribute) =
        let name =
            if attribute.name.NullOrEmpty() then
                GenText.SplitCamelCase(method.Name)
            else
                attribute.name

        Log.Message(taggify "DebugAction" method.Name (makeLabel(method, attribute)))
        true

    let Transpiler (instructions: CodeInstruction seq) =
        let insts = List.ofSeq instructions

        // instructions before ldarg.2
        let first, others =
            insts
            |> List.findIndex
                (fun inst ->
                    inst.opcode = OpCodes.Ldfld
                    && (inst.operand :?> FieldInfo) = AccessTools.Field(typeof<DebugActionAttribute>, "name"))
            |> add -1
            |> splitFlipped insts

        let originalJumpDest, others' = headTail others
        let jumpLabels = SysList<Label>(originalJumpDest.labels)
        do originalJumpDest.labels.Clear()

        let loadArg1 = CodeInstruction(OpCodes.Ldarg_1)
        do loadArg1.labels <- jumpLabels

        // skip instructions until the next stloc.0
        let rest =
            others'
            |> List.findIndex (fun inst -> inst.opcode = OpCodes.Stloc_0)
            |> splitFlipped others'
            |> snd

        seq {
            yield! first
            yield loadArg1
            yield CodeInstruction(OpCodes.Ldarg_2)
            yield CodeInstruction(OpCodes.Call, AccessTools.Method(typeof<IMarker>.DeclaringType, "makeLabel"))
            yield! rest
        }
