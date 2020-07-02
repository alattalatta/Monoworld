module DevL10N.Harmonize.EditWindow_Log

open System.Reflection
open System.Reflection.Emit

open Poet.Lyric.Translation
open HarmonyLib
open Verse

open DevL10N.Lib


[<HarmonyPatch(typeof<EditWindow_Log>, "DoWindowContents")>]
module DoWindowContents =
    let private stringToKey str =
        match str with
        | "Clear" -> "LogWindow_Clear"
        | "Trace big" -> "LogWindow_TraceBig"
        | "Trace medium" -> "LogWindow_TraceMedium"
        | "Trace small" -> "LogWindow_TraceSmall"
        | "Auto-open is ON" -> "LogWindow_AutoOpenEnabled"
        | "Auto-open is OFF" -> "LogWindow_AutoOpenDisabled"
        | "Copy to clipboard" -> "LogWindow_CopyToClipboard"
        | _ ->
            do Log.Warning
                ("[Dev In Your Language] Cannot find key for the following string:\n"
                 + str)
            str

    let Transpiler (instructions: CodeInstruction seq) =
        let firstInst = Seq.head instructions
        let insts = Seq.pairwise instructions

        seq {
            yield firstInst
            for (prev, cur) in insts do
                if prev.opcode = OpCodes.Ldstr
                   && cur.opcode = OpCodes.Ldstr then
                    let prevKey = stringToKey (prev.operand :?> string)
                    yield CodeInstruction(OpCodes.Ldstr, translate (prevKey + ".Description"))
                elif cur.opcode = OpCodes.Ldstr then
                    yield CodeInstruction(OpCodes.Ldstr, translate (stringToKey (cur.operand :?> string)))
                else
                    yield cur
        }
