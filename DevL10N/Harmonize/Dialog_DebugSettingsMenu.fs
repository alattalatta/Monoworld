module DevL10N.Harmonize.Dialog_DebugSettingsMenu

open System.Reflection

open HarmonyLib
open LudeonTK
open Verse

#if DEBUG
open DevL10N.Lib
#endif

let supportedAssemblies = [ "Assembly-CSharp"; "DevL10N" ]


[<HarmonyPatch(typeof<DebugTabMenu_Settings>, "LegibleFieldName")>]
module LegibleFieldName =
  let Prefix (fi: FieldInfo, __result: outref<string>) =
    if List.contains (fi.DeclaringType.Assembly.GetName().Name) supportedAssemblies then
#if DEBUG
      Log.Message(taggify "" (fi.Name.CapitalizeFirst()) (fi.Name.CapitalizeFirst().TranslateSimple()))
#endif
      do __result <- fi.Name.CapitalizeFirst().TranslateSimple()
      false

    else
      true
