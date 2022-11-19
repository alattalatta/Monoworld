module DevL10N.TranslationOutput.Utils.Translation

open HarmonyLib
open Verse


let private _getSourceFile =
  AccessTools.Method(typeof<TranslationFilesCleaner>, "GetSourceFile")

let getSourceFile (def: Def) =
  _getSourceFile.Invoke(null, [| def |]) |> string


let suggestTKeyPath normalizedPath =
  let mutable result: string = null

  if (TKeySystem.TrySuggestTKeyPath(normalizedPath, &result)) then
    Some result
  else
    None
