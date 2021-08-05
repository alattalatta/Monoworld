namespace DevL10N.TranslationOutput.Utils

open System.IO
open System.Text
open System.Xml.Linq

open HarmonyLib
open Verse


module Translation =
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

  let saveXMLDocument (path: string) (doc: XDocument) =
    async {
      use file = File.Create(path)

      let contentsString =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n"
        + doc
          .ToString()
          .Replace("<!--BR-->", "") // exploit the fact that the comments make new lines in .ToString()
          .Replace("&gt;", ">")
        |> Encoding.UTF8.GetBytes

      try
        do! file.AsyncWrite(contentsString, 0, contentsString.Length)
      with
      | ex -> Log.Error(ex.Message)
    }

  let suggestTKeyPath normalizedPath =
    let mutable result: string = null

    if (TKeySystem.TrySuggestTKeyPath(normalizedPath, &result)) then
      Some result
    else
      None
