module DevL10N.Harmonize.Dialog_DebugSettingsMenu

open System.Reflection

open HarmonyLib
open Verse

open DevL10N.Lib


[<HarmonyPatch(typeof<DebugTabMenu_Settings>, "LegibleFieldName")>]
module DoField =
  let Prefix (fi: FieldInfo, __result: outref<string>) =
    let name =
      GenText
        .SplitCamelCase(fi.Name)
        .ToLower()
        .CapitalizeFirst()

#if DEBUG
    Log.Message(taggify "" (fi.Name.CapitalizeFirst()) (fi.Name.CapitalizeFirst().TranslateSimple()))
#endif

    do __result <- fi.Name.CapitalizeFirst().TranslateSimple()

    false
