module DevL10N.TranslationOutput.Utils.Translation

open HarmonyLib
open Verse


let private _getSourceFile =
  AccessTools.Method(typeof<TranslationFilesCleaner>, "GetSourceFile")

let private _sanitizeXComment =
  AccessTools.Method(typeof<TranslationFilesCleaner>, "SanitizeXComment")

let getSourceFile (def: Def) =
  _getSourceFile.Invoke(null, [| def |]) |> string

let sanitizeXComment (str: string) =
  _sanitizeXComment.Invoke(null, [| str |])
  |> string

let getLanguageFolder (modData: ModMetaData) (lang: LoadedLanguage) =
  (TranslationFilesCleaner.GetLanguageFolderPath(lang, modData.RootDir.FullName))

let tryGetTextFromKey (curLang: LoadedLanguage) (key: string) =
  let res = ref (TaggedString())

  if curLang.TryGetTextFromKey(key, res) then
    Some(string res.Value)
  else
    None

let suggestTKeyPath normalizedPath =
  let mutable result: string = null

  if (TKeySystem.TrySuggestTKeyPath(normalizedPath, &result)) then
    Some result
  else
    None
