module DevL10N.TranslationOutput.BackstoriesOutput

open System.IO
open System.Xml.Linq

open Poet.Lib
open RimWorld
open Verse

open DevL10N.TranslationOutput.Utils
open DevL10N.TranslationOutput.Utils.Xml


let private createFormattedFieldElement (fieldName: string, value: string, english: string) : XNode list =
  if (value.NullOrEmpty()) && (english.NullOrEmpty()) then
    List.empty
  else
    [ if not (english.NullOrEmpty()) then
        yield XComment(Translation.sanitizeXComment (" EN: " + english.Replace("\n", "\\n") + " "))

      yield XElement.createSingleton fieldName (XText(value)) ]


let private createFormattedBackstoryElement (key: string, backstory: Backstory) : XNode =
  let fieldElements =
    seq {
      yield! createFormattedFieldElement ("title", backstory.title, backstory.untranslatedTitle)
      yield! createFormattedFieldElement ("titleFemale", backstory.titleFemale, backstory.untranslatedTitleFemale)
      yield! createFormattedFieldElement ("titleShort", backstory.titleShort, backstory.untranslatedTitleShort)

      yield!
        createFormattedFieldElement (
          "titleShortFemale",
          backstory.titleShortFemale,
          backstory.untranslatedTitleShortFemale
        )

      yield! createFormattedFieldElement ("desc", backstory.baseDesc, backstory.untranslatedDesc)
    }

  (XElement.create key fieldElements) :> XNode


let cleanup () =
  let coreMod =
    LoadedModManager.RunningMods
    |> Seq.find (fun modData -> modData.IsCoreMod)

  let getLanguageFolderPathFor =
    Translation.getLanguageFolder coreMod.ModMetaData

  let curLang = LanguageDatabase.activeLanguage

  let backstoriesDirPath =
    Path.Combine((getLanguageFolderPathFor curLang), "Backstories")

  Directory.CreateDirectory backstoriesDirPath
  |> ignore

  let backstoriesFilePath =
    Path.Combine(backstoriesDirPath, "Backstories.xml")

  do File.Delete backstoriesFilePath

  BackstoryDatabase.allBackstories
  |> dictseq
  |> Seq.sortWith (fun a b -> a.Key.CompareTo(b.Key))
  |> Seq.map (fun p -> createFormattedBackstoryElement (p.Key, p.Value))
  |> XElement.create "BackstoryTranslations"
  |> id<XNode>
  |> XDocument.createSingleton
  |> Translation.saveXMLDocument backstoriesFilePath
