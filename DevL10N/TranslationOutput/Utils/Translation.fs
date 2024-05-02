module DevL10N.TranslationOutput.Utils.Translation

open System.Reflection

open HarmonyLib
open Verse


let private _getSourceFile =
  AccessTools.Method(typeof<TranslationFilesCleaner>, "GetSourceFile")

let getSourceFile (def: Def) =
  _getSourceFile.Invoke(null, [| def |]) |> string


// fields that should be translated but without [MustTranslate]
let private additionalMustTranslateList = set [
  ("DeityNameType", "name");
  ("HediffCompProperties_TendDuration", "labelSolidTendedWell");
  ("HediffCompProperties_TendDuration", "labelTendedWell");
  ("HediffCompProperties_TendDuration", "labelTendedWellInner");
  ("HistoryAutoRecorderDef", "valueFormat");
  ("InjuryProps", "destroyedLabel");
  ("InjuryProps", "destroyedOutLabel");
  ("StuffProperties", "stuffAdjective");
]
 
let shouldInjectFor
  (def: Def, fieldInfo: FieldInfo, injectionMaybe: DefInjectionPackage.DefInjection option)
  (curValue: string)
  =
  Option.isSome injectionMaybe
  || DefInjectionUtility.ShouldCheckMissingInjection(curValue, fieldInfo, def)
  || Set.contains (fieldInfo.DeclaringType.Name, fieldInfo.Name) additionalMustTranslateList


let suggestTKeyPath normalizedPath =
  let mutable result: string = null

  if (TKeySystem.TrySuggestTKeyPath(normalizedPath, &result)) then
    Some result
  else
    None
