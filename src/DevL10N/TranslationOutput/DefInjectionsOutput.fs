module DevL10N.TranslationOutput.DefInjectionsOutput

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Xml.Linq

open Poet.Lyric.Translation
open Verse

open DevL10N.TranslationOutput.Utils
open DevL10N.TranslationOutput.Utils.Option
open DevL10N.TranslationOutput.Utils.Xml


type DefInjectionDict = IDictionary<string, DefInjectionPackage.DefInjection>

type DefInjectionTarget =
  { CurValue: string
    CurValueCollection: string list
    Def: Def
    FieldInfo: FieldInfo
    IsCollection: bool
    ListInjectionAllowed: bool
    NormalizedPath: string
    SuggestedPath: string }


module private Utils =
  let collectAllInjectionTargets (defType: Type, modData: ModMetaData) =
    let possibleDefInjections = ResizeArray<DefInjectionTarget>()

    let traverserBody
      suggestedPath
      normalizedPath
      isCollection
      curValue
      curValueCollection
      translationAllowed
      fullListTranslationAllowed
      fieldInfo
      def
      =
      if translationAllowed then
        possibleDefInjections.Add(
          { CurValue = curValue
            CurValueCollection =
              if isNull curValueCollection then
                List.empty
              else
                List.ofSeq curValueCollection
            Def = def
            FieldInfo = fieldInfo
            IsCollection = isCollection
            ListInjectionAllowed = fullListTranslationAllowed
            NormalizedPath = normalizedPath
            SuggestedPath = suggestedPath }
        )

    let traverser =
      DefInjectionUtility.PossibleDefInjectionTraverser(traverserBody)

    do DefInjectionUtility.ForEachPossibleDefInjection(defType, traverser, modData)

    possibleDefInjections


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
    let injectionValue =
      injection.fullListInjection |> List.ofSeq

    let length =
      min (List.length injectionValue) (List.length english)

    // If the injection is shorter than the english, we need to truncate it.
    let (used, unused) =
      english |> List.ofSeq |> List.splitAt length

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
  (injectionTarget: DefInjectionTarget)
  (injectionMaybe: DefInjectionPackage.DefInjection option)
  (english: string)
  =
  Option.isSome injectionMaybe
  || (not (english.NullOrEmpty())
      && DefInjectionUtility.ShouldCheckMissingInjection(english, injectionTarget.FieldInfo, injectionTarget.Def))


let private createInjectionCollectionElements
  (
    languageInjections: DefInjectionDict,
    target: DefInjectionTarget
  ) : XNode list =
  let injectionMaybe =
    languageInjections.TryGetValue(target.SuggestedPath, null)
    |> Option.ofObj

  let englishList =
    injectionMaybe
    |> Option.filter (fun injection -> injection.injected)
    |> Option.map (fun injection -> injection.replacedList)
    |> Option.map List.ofSeq
    |> Option.defaultValue (
      target.CurValueCollection
      |> List.mapi
           (fun i value ->
             let key =
               target.NormalizedPath + "." + i.ToString()

             let path =
               Translation.suggestTKeyPath key
               |> Option.defaultValue (key)

             languageInjections.TryGetValue(path, null)
             |> Option.ofObj
             |> Option.filter (fun injection -> injection.injected)
             |> Option.map (fun injection -> injection.replacedString)
             |> Option.defaultValue value)
    )

  if target.ListInjectionAllowed then
    maybe {
      let mayProceed =
        englishList
        |> List.exists (shouldInjectFor target injectionMaybe)

      if mayProceed then
        return createListInjectionElements (injectionMaybe, englishList)
    }
    |> Option.map (XElement.create target.SuggestedPath)
    |> Option.map (id<XNode> >> List.singleton)
    |> Option.defaultValue List.empty
  else
    englishList
    |> Seq.collecti
         (fun (i, english) ->
           let normalizedPath =
             target.NormalizedPath + "." + i.ToString()

           let suggestedPath =
             Translation.suggestTKeyPath normalizedPath
             |> Option.defaultValue (target.SuggestedPath + "." + i.ToString())

           createSingleInjectionElement (suggestedPath, english)
           |> Seq.singleton)
    |> List.ofSeq


let private createInjectionElement (languageInjections: DefInjectionDict, target: DefInjectionTarget) =
  let injectionMaybe =
    languageInjections.TryGetValue(target.SuggestedPath, null)
    |> Option.ofObj

  injectionMaybe
  |> Option.filter (fun x -> x.injected)
  |> Option.bind (fun x -> Option.ofObj x.replacedString)
  |> Option.orElse (Option.ofObj target.CurValue)
  |> Option.filter (shouldInjectFor target injectionMaybe)
  |> Option.map (fun english -> createSingleInjectionElement (target.SuggestedPath, english))


let private writeFile
  (
    languageInjections: DefInjectionDict,
    allInjectionTargets: DefInjectionTarget list,
    defType: Type,
    defInjectionDirPath: string,
    fileName: string
  ) =
  async {
    let injectionTargetsInFile =
      allInjectionTargets
      |> List.filter (fun (injection) -> (Translation.getSourceFile injection.Def) = fileName)

    let injectionTargetsCount = List.length injectionTargetsInFile

    if injectionTargetsCount > 0 then
      let injectionsToWrite =
        injectionTargetsInFile
        |> List.map (fun injection -> injection.Def.defName)
        |> List.distinct
        |> List.sort
        |> List.map
             (fun defName ->
               injectionTargetsInFile
               |> List.filter (fun injectionTarget -> injectionTarget.Def.defName = defName)
               |> List.collect
                    (fun injectionTarget ->
                      try
                        if injectionTarget.IsCollection then
                          createInjectionCollectionElements (languageInjections, injectionTarget)
                        else
                          createInjectionElement (languageInjections, injectionTarget)
                          |> Option.map List.singleton
                          |> Option.defaultValue List.empty
                      with
                      | ex ->
                        do
                          Log.Error(
                            sprintf "Could not write injection for %s: %s" injectionTarget.SuggestedPath ex.Message
                          )

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
            defNamespace.StartsWith("RimWorld")
            || defNamespace.StartsWith("Verse")
          then
            defType.Name
          else
            defType.FullName

        let defTypeDirPath =
          Path.Combine(defInjectionDirPath, defTypeName)

        Directory.CreateDirectory(defTypeDirPath)
        |> ignore

        do!
          injectionsToWrite
          |> XElement.create "LanguageData"
          |> XDocument.createSingleton
          |> XDocument.save (Path.Combine(defTypeDirPath, fileName))
  }


let private writeForDefType (defInjectionDirPath: string, modData: ModMetaData) (defType: Type) =
  let languageInjections =
    LanguageDatabase.activeLanguage.defInjections
    |> Seq.filter (fun injectionPack -> injectionPack.defType = defType)
    |> Seq.collect
         (fun injectionPack ->
           injectionPack.injections
           |> Seq.filter
                (fun p ->
                  not p.Value.isPlaceholder
                  && p.Value.ModifiesDefFromModOrNullCore(modData, defType)))
    |> Seq.map (fun p -> (p.Key, p.Value))
    |> dict

  let allInjectionTargets =
    Utils.collectAllInjectionTargets (defType, modData)
    |> List.ofSeq

  // write each injection to its file
  allInjectionTargets
  |> Seq.map (fun injection -> Translation.getSourceFile injection.Def)
  |> Seq.distinct
  |> Seq.map
       (fun fileName ->
         async {
           try
             do! writeFile (languageInjections, allInjectionTargets, defType, defInjectionDirPath, fileName)
           with
           | ex ->
             Log.Error(
               "Could not write defInjection for "
               + defType.Name
               + ": "
               + ex.Message
             )
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
