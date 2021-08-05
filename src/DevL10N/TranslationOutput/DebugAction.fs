module DevL10N.TranslationOutput.DebugAction

open System
open System.IO

open HarmonyLib
open Poet.Lyric.Translation
open RimWorld
open Verse

open DevL10N.TranslationOutput


module private Reflectors =
  let private _cleanupBackstories =
    AccessTools.Method(typeof<TranslationFilesCleaner>, "CleanupBackstories")

  let cleanupBackstories () =
    _cleanupBackstories.Invoke(null, Array.empty)
    |> ignore


module private Utils =
  let isLanguageTarballed (language: LoadedLanguage) =
    language.AllDirectories
    |> Seq.exists (fun (dir, _, _) -> dir.FullPath.EndsWith ".tar") // TarDirectory is an internal class D:

  let pushMessage message =
    do Messages.Message(message, MessageTypeDefOf.RejectInput, false)


let private writeForMod (modData: ModMetaData) =
  // Desktop/TranslationOutput
  let destRoot =
    let desktop =
      Environment.GetFolderPath(Environment.SpecialFolder.Desktop)

    (if desktop.NullOrEmpty() then
       GenFilePaths.SaveDataFolderPath
     else
       desktop)
    |> (fun path -> Path.Combine(path, "TranslationOutput"))

  // Desktop/TranslationOutput/Core
  let dest =
    let modIdentifier =
      if modData.OnSteamWorkshop then
        sprintf "[%s] %s" modData.RootDir.Name modData.PackageId
      else
        modData.RootDir.Name

    Path.Combine(destRoot, modIdentifier)

  if Directory.Exists(dest) then
    do Directory.Delete(dest, true)

  Directory.CreateDirectory(dest) |> ignore

  let writeDefInjectionsAsync =
    async {
      try
        do! DefInjectionsOutput.write dest modData
      with
      | ex ->
        Log.Error(
          "Could not cleanup defInjected translations: "
          + ex.Message
        )
    }

  let writeKeysAsync =
    async {
      try
        do KeysOutput.write dest modData
      with
      | ex ->
        Log.Error(
          "Could not cleanup keyed translations: "
          + ex.Message
        )
    }

  async {
    do!
      [ writeDefInjectionsAsync
        writeKeysAsync ]
      |> Async.Parallel
      |> Async.Ignore

    do Utils.pushMessage (translate1 "DevL10N.TranslationOutput.SavedTo" dest)
  }



[<DebugAction("Translation", null, allowedGameStates = AllowedGameStates.Entry)>]
let outputTranslationSource () =
  let activeMods = ModsConfig.ActiveModsInLoadOrder
  let english = LanguageDatabase.defaultLanguage

  Some LanguageDatabase.activeLanguage
  |> Option.filter
       (fun curLang ->
         if Utils.isLanguageTarballed curLang then
           do Utils.pushMessage ((translate "MessageUnpackBeforeCleaningTranslationFiles"))

           false
         else
           true)
  |> Option.filter
       (fun curLang ->
         if curLang.anyKeyedReplacementsXmlParseError
            || curLang.anyDefInjectionsXmlParseError then
           let error =
             Option.ofObj curLang.lastKeyedReplacementsXmlParseErrorInFile
             |> Option.defaultValue curLang.lastDefInjectionsXmlParseErrorInFile

           do Utils.pushMessage ((translate1 "MessageCantCleanupTranslationFilesBeucaseOfXmlError" error))

           false
         else
           true)
  |> Option.iter
       (fun curLang ->
         let queueOutput (modData: ModMetaData) =
           do
             english.LoadData()
             curLang.LoadData()

             LongEventHandler.QueueLongEvent(
               (fun () -> writeForMod modData |> Async.RunSynchronously),
               (translate "DevL10N.TranslationOutput.Writing"),
               true,
               null
             )

         activeMods
         |> Seq.map
              (fun modData ->
                DebugMenuOption(modData.Name, DebugMenuOptionMode.Action, (fun () -> queueOutput modData)))
         |> Dialog_DebugOptionListLister
         |> Find.WindowStack.Add)
