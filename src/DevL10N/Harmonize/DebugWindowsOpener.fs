module DevL10N.Harmonize.DebugWindowsOpener

open System.Reflection.Emit

open Poet.Lyric.Translation
open HarmonyLib
open Verse

open DevL10N.Lib


[<HarmonyPatch(typeof<DebugWindowsOpener>, "DrawButtons")>]
module DrawButtons =
    let private stringToKeyed str =
        match str with
        | "Open the debug log." -> translate "DebugMenu_Logs"
        | "Open tweakvalues menu.\n\nThis lets you change internal values." -> translate "DebugMenu_TweakValues"
        | "Open the view settings.\n\nThis lets you see special debug visuals." -> translate "DebugMenu_ViewSettings"
        | "Open debug actions menu.\n\nThis lets you spawn items and force various events." ->
            translate "DebugMenu_DebugActions"
        | "Open debug logging menu." -> translate "DebugMenu_DebugLogging"
        | "Open the inspector.\n\nThis lets you inspect what's happening in the game, down to individual variables." ->
            translate "DebugMenu_Inspector"
        | "Toggle god mode.\n\nWhen god mode is on, you can build stuff instantly, for free, and sell things that aren't yours." ->
            translate "DebugMenu_GodMode"
        | "God mode" -> translate "GodMode"
        | "Pause the game when an error is logged." -> translate "DebugMenu_PauseOnError"
        | _ ->
            do Log.Warning
                ("[Dev In Your Language] Cannot find key for the following string:\n"
                 + str)
            str

    let Transpiler (instructions: CodeInstruction seq) =
        let insts = List.ofSeq instructions

        seq {
            for inst in insts do
                if inst.opcode = OpCodes.Ldstr then
                    let labels = SysList<Label>(inst.labels)
                    do inst.labels.Clear()

                    let mapped =
                        CodeInstruction(OpCodes.Ldstr, stringToKeyed (inst.operand :?> string))

                    do mapped.labels <- labels

                    yield mapped
                else
                    yield inst
        }
