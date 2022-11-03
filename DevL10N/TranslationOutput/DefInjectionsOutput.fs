module DevL10N.TranslationOutput.DefInjectionsOutput

open System
open System.Collections.Generic
open System.IO
open System.Xml.Linq

open Poet.Lyric.Console
open Poet.Lyric.Translation
open Verse

open DevL10N.TranslationOutput.Injectable
open DevL10N.TranslationOutput.Utils
open DevL10N.TranslationOutput.Utils.Option
open DevL10N.TranslationOutput.Utils.Xml
open RimWorld


type DefInjectionDict = IDictionary<string, DefInjectionPackage.DefInjection>


let private createListItems (strs: string list) : XNode list =
  strs
  |> List.map (fun str -> (XElement.createSingleton "li" (XText(str.Replace("\n", "\\n")))) :> XNode)


let private createListInjectionElements
  (
    injectionMaybe: DefInjectionPackage.DefInjection option,
    english: string list
  ) : XNode list =
  match injectionMaybe with
  | Some injection ->
    let injectionValue = injection.fullListInjection |> List.ofSeq

    let length = min (List.length injectionValue) (List.length english)

    // If the injection is shorter than the english, we need to truncate it.
    let (used, unused) = english |> List.ofSeq |> List.splitAt length

    // If the injection is longer than the english, we need to add extras from it.
    let extraItemsFromInjection =
      if length < List.length injectionValue then
        injectionValue |> List.splitAt length |> snd
      else
        List.empty

    [ yield! createListItems used
      if not (List.isEmpty extraItemsFromInjection) then
        yield
          XComment(translate1 "DevL10N.TranslationOutput.ExtraItemsFromLanguage" (List.length extraItemsFromInjection))

        yield! createListItems extraItemsFromInjection
      if not (List.isEmpty unused) then
        yield XComment(translate1 "DevL10N.TranslationOutput.RemovedItemsFromLanguage" (List.length unused)) ]
  | None -> createListItems english


let private createSingleInjectionElement (path: string, english: string) : XNode =
  if
    path.EndsWith(".slateRef")
    && ConvertHelper.IsXml(english)
  then
    XElement.Parse(sprintf "<%s>%s</%s>" path english path) :> XNode
  else
    XText(english.Replace("\n", "\\n"))
    |> XElement.createSingleton path
    |> id<XNode>


let private shouldInjectFor
  (injectable: Injectable)
  (injectionMaybe: DefInjectionPackage.DefInjection option)
  (curValue: string)
  =
  Option.isSome injectionMaybe
  || DefInjectionUtility.ShouldCheckMissingInjection(curValue, injectable.FieldInfo, injectable.Def)
  || DefInjectionUtility.ShouldCheckMissingInjection(curValue, injectionTarget.FieldInfo, injectionTarget.Def)


let private createInjectionCollectionElements
  (
    injections: DefInjectionDict,
    injectable: Injectable,
    (value, canTranslateList): (string list * bool)
  ) : XNode list =
  let injectionMaybe =
    injections.TryGetValue(injectable.SuggestedPath, null)
    |> Option.ofObj

  let englishList =
    injectionMaybe
    |> Option.filter (fun injection -> injection.injected)
    |> Option.map (fun injection -> injection.replacedList)
    |> Option.map List.ofSeq
    |> Option.defaultValue (
      value
      |> List.mapi (fun i value ->
        let key = injectable.NormalizedPath + "." + i.ToString()

        let path =
          Translation.suggestTKeyPath key
          |> Option.defaultValue (key)

        injections.TryGetValue(path, null)
        |> Option.ofObj
        |> Option.filter (fun injection -> injection.injected)
        |> Option.map (fun injection -> injection.replacedString)
        |> Option.defaultValue value)
    )

  if canTranslateList then
    maybe {
      let mayProceed =
        englishList
        |> List.exists (shouldInjectFor injectable injectionMaybe)

      if mayProceed then
        return createListInjectionElements (injectionMaybe, englishList)
    }
    |> Option.map (XElement.create injectable.SuggestedPath)
    |> Option.map (id<XNode> >> List.singleton)
    |> Option.defaultValue List.empty
  else
    englishList
    |> List.ofSeq
    |> List.collecti (fun (i, english) ->
      if shouldInjectFor injectable injectionMaybe english then
        let normalizedPath = injectable.NormalizedPath + "." + i.ToString()

        let suggestedPath =
          Translation.suggestTKeyPath normalizedPath
          |> Option.defaultValue (injectable.SuggestedPath + "." + i.ToString())

        createSingleInjectionElement (suggestedPath, english)
        |> List.singleton
      else
        List.empty)


let private createInjectionElement (injections: DefInjectionDict, target: Injectable, value: string) =
  let injectionMaybe =
    injections.TryGetValue(target.SuggestedPath, null)
    |> Option.ofObj

  injectionMaybe
  |> Option.filter (fun x -> x.injected)
  |> Option.bind (fun x -> Option.ofObj x.replacedString)
  |> Option.orElse (Option.ofObj value)
  |> Option.filter (shouldInjectFor target injectionMaybe)
  |> Option.map (fun english -> createSingleInjectionElement (target.SuggestedPath, english))


let private writeFile
  (
    injections: DefInjectionDict,
    injectables: Injectable list,
    defType: Type,
    defInjectionDirPath: string,
    fileName: string
  ) =
  async {
    let injectablesInFile =
      injectables
      |> List.filter (fun (injection) -> (Translation.getSourceFile injection.Def) = fileName)

    if not (List.isEmpty injectablesInFile) then
      let injectionsToWrite =
        injectablesInFile
        |> List.map (fun injectable -> injectable.Def.defName)
        |> List.distinct
        |> List.sort
        |> List.map (fun defName ->
          injectablesInFile
          |> List.filter (fun injectable -> injectable.Def.defName = defName)
          |> List.collect (fun injectable ->
            if defType = typeof<FactionDef> then
              do log "Doing %s\n%A" injectable.SuggestedPath injectable

            try
              match injectable.Value with
              | Injectable.Collection c -> createInjectionCollectionElements (injections, injectable, c)
              | Injectable.Singular s ->
                createInjectionElement (injections, injectable, s)
                |> Option.map List.singleton
                |> Option.defaultValue List.empty
            with
            | ex ->
              do
                err
                  "Could not write injection for %s in %s, path '%s': %s\nInjectable: %A"
                  defType.Name
                  fileName
                  injectable.SuggestedPath
                  ex.Message
                  injectable

              List.empty))
        |> List.filter (List.isEmpty >> not)
        |> List.intersperse ([ XComment.newLine () :> XNode ])
        |> List.concat

      if not (List.isEmpty injectionsToWrite) then
        let defTypeName =
          let defNamespace = defType.Namespace

          // RimWorld.ThingDef -> ThingDef
          // RimWorld.AI.DutyDef -> DutyDef
          // Infusion.InfusionDef -> Infusion.InfusionDef
          if
            defNamespace.NullOrEmpty()
            || defNamespace.StartsWith("RimWorld")
            || defNamespace.StartsWith("Verse")
          then
            defType.Name
          else
            defType.FullName

        let defTypeDirPath = Path.Combine(defInjectionDirPath, defTypeName)

        do Directory.CreateDirectory(defTypeDirPath)
           |> ignore

        do!
          injectionsToWrite
          |> XElement.create "LanguageData"
          |> XDocument.createSingleton
          |> XDocument.save (Path.Combine(defTypeDirPath, fileName))
  }


let private writeForDefType (defInjectionDirPath: string, modData: ModMetaData) (defType: Type) =
  let injections =
    LanguageDatabase.activeLanguage.defInjections
    |> Seq.filter (fun injectionPack -> injectionPack.defType = defType)
    |> Seq.collect (fun injectionPack ->
      injectionPack.injections
      |> Seq.filter (fun p ->
        not p.Value.isPlaceholder
        && p.Value.ModifiesDefFromModOrNullCore(modData, defType)))
    |> Seq.map (fun p -> (p.Key, p.Value))
    |> dict

  let injectables = Injectable.collectAllFor defType modData

  // write each injection to its file
  injectables
  |> Seq.map (fun injectable -> Translation.getSourceFile injectable.Def)
  |> Seq.distinct
  |> Seq.map (fun fileName ->
    async {
      try
        do! writeFile (injections, injectables, defType, defInjectionDirPath, fileName)
      with
      | ex -> do err "Could not write defInjection for %s in %s: %s" defType.Name fileName ex.Message
    })
  |> Async.Parallel


let write basePath (modData: ModMetaData) =
  let defInjectionDirInfo =
    Path.Combine(basePath, "DefInjected")
    |> DirectoryInfo

  if not defInjectionDirInfo.Exists then
    do defInjectionDirInfo.Create()

  async {
    let modContentPack =
      LoadedModManager.RunningMods
      |> Seq.find (fun m -> m.PackageId = modData.PackageId)

    do!
      modContentPack.AllDefs
      |> Seq.groupBy (fun def -> def.GetType())
      |> Seq.map (
        fst
        >> writeForDefType (defInjectionDirInfo.FullName, modData)
      )
      |> Async.Parallel
      |> Async.Ignore
  }
