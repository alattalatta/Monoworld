module DevL10N.TranslationOutput.KeysOutput

open System
open System.IO
open System.Xml.Linq

open Verse

open DevL10N.TranslationOutput.Utils


let private replaceElement (curLang: LoadedLanguage) (parent: XElement) =
  let descendants = parent.DescendantNodes() |> List.ofSeq

  // get original
  let englishValueMaybe =
    descendants
    |> List.tryHead
    |> Option.bind
         (fun node ->
           match node with
           | :? XText as text -> Some text.Value
           | _ -> None)

  // remove contents
  do
    descendants
    |> List.iter
         (fun child ->
           if not (isNull child) then
             do child.Remove())

  // write translation
  do
    englishValueMaybe
    |> Option.iter
         (fun englishValue ->
           // write comment
           let comment = " EN: " + englishValue + " "
           do parent.AddBeforeSelf(XComment(Translation.sanitizeXComment (comment)))
           do parent.AddBeforeSelf(Environment.NewLine)
           do parent.AddBeforeSelf("  ")

           // write translation
           match Translation.tryGetTextFromKey curLang (string parent.Name) with
           | Some text -> do parent.Add(XText(text.Replace("\n", "\\n")))
           | None -> do parent.Add(XText(englishValue)))


let write basePath (modData: ModMetaData) =
  let english = LanguageDatabase.defaultLanguage

  let keyedDirInfo =
    Path.Combine(basePath, "Keyed") |> DirectoryInfo

  let englishKeyedDirInfo =
    Path.Combine((Translation.getLanguageFolder modData english), "Keyed")
    |> DirectoryInfo

  if not englishKeyedDirInfo.Exists then
    Log.Warning(
      "English keyed translations folder doesn't exist for "
      + modData.Name
      + ", skipping..."
    )
  else
    if not keyedDirInfo.Exists then
      do keyedDirInfo.Create()

    do
      // delete existing ones
      keyedDirInfo.GetFiles("*.xml", SearchOption.AllDirectories)
      |> Array.iter
           (fun fileInfo ->
             try
               fileInfo.Delete()
             with
             | ex ->
               Log.Error(
                 "Could not delete "
                 + fileInfo.FullName
                 + ": "
                 + ex.Message
               ))

      // copy english files to dest
      englishKeyedDirInfo.GetFiles("*.xml", SearchOption.AllDirectories)
      |> Array.iter
           (fun fileInfo ->
             let path =
               Uri(
                 englishKeyedDirInfo.FullName
                 + Path.DirectorySeparatorChar.ToString()
               )
                 .MakeRelativeUri(Uri(fileInfo.FullName))
                 .ToString()

             let destination =
               Path.Combine(keyedDirInfo.FullName, path)

             try
               Directory.CreateDirectory(Path.GetDirectoryName(destination))
               |> ignore

               fileInfo.CopyTo(destination) |> ignore
             with
             | ex ->
               Log.Error(
                 "Could not copy "
                 + fileInfo.FullName
                 + ": "
                 + ex.Message
               ))

// do
//     keyedDirInfo.GetFiles("*.xml", SearchOption.AllDirectories)
//     |> Array.iter
//         (fun fileInfo ->
//             let doc =
//                 XDocument.Load(fileInfo.FullName, LoadOptions.PreserveWhitespace)

//             let keyElements =
//                 doc.DescendantNodes()
//                 |> Seq.ofType<XNode, XElement>
//                 |> Seq.tryHead
//                 |> Option.map
//                     (fun root ->
//                         root.DescendantNodes()
//                         |> Seq.ofType<XNode, XElement>)
//                 |> Option.defaultValue Seq.empty

//             keyElements |> Seq.iter (replaceElement curLang)

//             do Translation.saveXMLDocument fileInfo.FullName doc)
// |> Async.Parallel
// |> Async.Ignore
