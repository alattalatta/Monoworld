module DevL10N.TranslationOutput.Copier

open System.IO

open Verse


let rec copyRec (srcDir: DirectoryInfo) (destDir: DirectoryInfo) =
  if not destDir.Exists then
    destDir.Create() |> ignore

  srcDir.EnumerateDirectories()
  |> Seq.iter (fun di ->
    let srcDirSub = new DirectoryInfo(Path.Combine(srcDir.FullName, di.Name))
    let destDirSub = new DirectoryInfo(Path.Combine(destDir.FullName, di.Name))

    copyRec srcDirSub destDirSub)

  srcDir.EnumerateFiles()
  |> Seq.iter (fun file ->
    Path.Combine(destDir.FullName, file.Name)
    |> file.CopyTo
    |> ignore)


let copy destPath (modData: ModMetaData) name =
  let srcDir =
    new DirectoryInfo(
      Path.Combine(
        TranslationFilesCleaner.GetLanguageFolderPath(LanguageDatabase.defaultLanguage, string modData.RootDir),
        name
      )
    )

  if srcDir.Exists then
    copyRec srcDir (new DirectoryInfo(Path.Combine(destPath, name)))
