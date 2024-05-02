module DevL10N.TranslationOutput.DefInjectionsOutput

open System
open System.Collections.Generic
open System.IO
open System.Xml.Linq

open Poet.Lib
open Poet.Lyric.Console
open Verse

open DevL10N.TranslationOutput.Injectable
open DevL10N.TranslationOutput.Utils
open DevL10N.TranslationOutput.Utils.Option
open DevL10N.TranslationOutput.Utils.Xml


type DefInjectionDictByNormPath = IDictionary<string, DefInjectionPackage.DefInjection>


let private createListXMLItems (strs: string list) : XNode list =
  strs
  |> List.map (fun str -> (XElement.createSingleton "li" (XText(str.Replace("\n", "\\n")))) :> XNode)


let private createListXMLElement (english: string list) : XNode list = createListXMLItems english


let private createSingularXMLElement (path: string, english: string) : XNode =
  if
    path.EndsWith(".slateRef")
    && ConvertHelper.IsXml(english)
  then
    XElement.Parse(sprintf "<%s>%s</%s>" path english path) :> XNode
  else
    XText(english.Replace("\n", "\\n"))
    |> XElement.createSingleton path
    |> id<XNode>


let private createInjectionsForCollection
  (
    injections: DefInjectionDictByNormPath,
    injectable: Injectable,
    canTranslateList: bool
  ) : XNode list =
  let injection = Dict.get injectable.NormalizedPath injections

  let englishList =
    injection
    |> Option.filter (fun injection -> injection.injected)
    |> Option.map (fun injection -> injection.replacedList)
    |> Option.map List.ofSeq
    |> Option.defaultWith (fun () ->
      let rec collectListItemsRec (prev: string seq, i: int) =
        let key = injectable.NormalizedPath + "." + i.ToString()
        let injectionIndexed = Dict.get key injections

        injectionIndexed
        |> Option.filter (fun injection -> injection.injected)
        |> Option.map (fun injection -> injection.replacedString)
        |> Option.map (fun english ->
          seq {
            yield english
            yield! collectListItemsRec (prev, i + 1)
          })
        |> Option.defaultValue prev

      collectListItemsRec (Seq.empty, 0) |> List.ofSeq)

  if canTranslateList then
    maybe {
      let mayProceed =
        englishList
        |> List.exists (Translation.shouldInjectFor (injectable.Def, injectable.FieldInfo, injection))

      if mayProceed then
        return
          createListXMLElement englishList
          |> XElement.create injectable.SuggestedPath
          |> (id<XNode> >> List.singleton)
    }
    |> Option.defaultValue List.empty
  else
    englishList
    |> List.ofSeq
    |> List.collecti (fun (i, english) ->
      if Translation.shouldInjectFor (injectable.Def, injectable.FieldInfo, injection) english then
        let normPath = injectable.NormalizedPath + "." + i.ToString()

        let suggPath =
          Translation.suggestTKeyPath normPath
          |> Option.defaultValue (injectable.SuggestedPath + "." + i.ToString())

        createSingularXMLElement (suggPath, english)
        |> List.singleton
      else
        List.empty)


let private createInjection (injections: DefInjectionDictByNormPath, injectable: Injectable, curValue: string) =
  let injection = Dict.get injectable.NormalizedPath injections

  let english =
    injection
    |> Option.filter (fun x -> x.injected)
    |> Option.bind (fun x -> Option.ofObj x.replacedString)
    |> Option.defaultValue (curValue)

  if Translation.shouldInjectFor (injectable.Def, injectable.FieldInfo, injection) english then
    Some(createSingularXMLElement (injectable.SuggestedPath, english))
  else
    None


let private writeFile
  (
    injections: DefInjectionDictByNormPath,
    injectables: Injectable list,
    defType: Type,
    defInjectionDirPath: string,
    fileName: string
  ) =
  async {
    if not (List.isEmpty injectables) then
      let injectionsToWrite =
        injectables
        |> List.groupBy (fun injectable -> injectable.Def.defName)
        |> List.distinctBy fst // group by defName
        |> List.sortBy fst // sort by defName'
        // not collect, we want injections to be grouped by defName, defNames separated by a new line
        |> List.map (fun (_, injectablesForDef) ->
          injectablesForDef
          |> List.collect (fun injectable ->
            try
              match injectable.Value with
              | Injectable.Collection canTranslateList ->
                createInjectionsForCollection (injections, injectable, canTranslateList)
              | Injectable.Singular curValue ->
                curValue
                |> Option.bind (fun cv -> createInjection (injections, injectable, cv))
                |> Option.map List.singleton
                |> Option.defaultValue List.empty
            with
            | ex ->
              err
                "Could not write injection for %s in %s, path '%s': %s\nInjectable: %A"
                defType.Name
                fileName
                injectable.SuggestedPath
                ex.Message
                injectable

              List.empty))
        |> List.filter (List.isEmpty >> not)
        // the separation
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

        Directory.CreateDirectory(defTypeDirPath)
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
    |> Seq.filter (fun injectionPacks -> injectionPacks.defType = defType)
    |> Seq.collect (fun injectionPacks ->
      injectionPacks.injections
      |> Seq.filter (fun p ->
        not p.Value.isPlaceholder
        && p.Value.ModifiesDefFromModOrNullCore(modData, defType)))
    |> Seq.map (fun p -> (p.Value.normalizedPath, p.Value))
    |> dict

  Injectable.collectAllFor defType modData
  |> List.groupBy (fun injectable -> (Translation.getSourceFile injectable.Def))
  |> Seq.map (fun (fileName, injectables) ->
    async {
      try
        do! writeFile (injections, injectables, defType, defInjectionDirPath, fileName)
      with
      | ex -> err "Could not write defInjection for %s in %s: %s" defType.Name fileName ex.Message
    })
  |> Async.Parallel


let write destPath (modData: ModMetaData) =
  let defInjectionDirInfo =
    Path.Combine(destPath, "DefInjected")
    |> DirectoryInfo

  if not defInjectionDirInfo.Exists then
    defInjectionDirInfo.Create()

  async {
    do!
      GenDefDatabase.AllDefTypesWithDatabases()
      |> Seq.map (writeForDefType (defInjectionDirInfo.FullName, modData))
      |> Async.Parallel
      |> Async.Ignore
  }
