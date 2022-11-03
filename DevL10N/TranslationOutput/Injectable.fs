module DevL10N.TranslationOutput.Injectable

open System
open System.Collections.Generic
open System.Reflection

open HarmonyLib
open Verse

open DevL10N.TranslationOutput.Utils


let private _fieldsInDeterministicOrder =
  AccessTools.Method(typeof<DefInjectionUtility>, "FieldsInDeterministicOrder")

let private fieldsInDeterministicOrder (t: Type) =
  _fieldsInDeterministicOrder.Invoke(null, [| t |]) :?> FieldInfo seq



type InjectableValue =
  | Collection of (string list * bool) // values, [TranslationCanCahngeCount]
  | Singular of string

type Injectable =
  { Def: Def
    FieldInfo: FieldInfo
    NormalizedPath: string
    SuggestedPath: string
    Value: InjectableValue }

let makePath prevNormPath prevSuggestedPath (field: FieldInfo) =
  let newNormPath = prevNormPath + "." + field.Name

  let newSuggestedPath =
    let mutable tKeyPath: string = null

    if TKeySystem.TrySuggestTKeyPath(newNormPath, ref tKeyPath) then
      tKeyPath
    else
      prevSuggestedPath + "." + field.Name

  (newNormPath, newSuggestedPath)


let rec private collectInDefRecursive
  (
    obj: obj,
    normPath: string,
    suggestedPath: string,
    def: Def,
    visited: HashSet<Object>,
    acc: Injectable seq
  ) =
  let objType = obj.GetType()

  if isNull obj
     || obj :? Thing
     || (not (objType.IsValueType) && visited.Contains(obj)) then
    (visited, acc)
  else
    do visited.Add(obj) |> ignore

    (visited,
     seq {
       yield! acc

       yield!
         fieldsInDeterministicOrder (objType)
         |> Seq.collect (fun field ->
           let (newNormPath, newSuggestedPath) = makePath normPath suggestedPath field
           let value = field.GetValue(obj)

           if field.HasAttribute<NoTranslateAttribute>()
              || field.HasAttribute<UnsavedAttribute>()
              || value :? Def then
             Seq.empty

           // string
           else if typeof<string>.IsAssignableFrom (field.FieldType) then
             Seq.singleton (
               { Def = def
                 FieldInfo = field
                 NormalizedPath = newNormPath
                 SuggestedPath = newSuggestedPath
                 Value = InjectableValue.Singular(value :?> string) }
             )

           // string collection
           else if value :? string seq then
             Seq.singleton (
               { Def = def
                 FieldInfo = field
                 NormalizedPath = newNormPath
                 SuggestedPath = newSuggestedPath
                 Value =
                   InjectableValue.Collection(
                     value :?> string seq |> List.ofSeq,
                     field.HasAttribute<TranslationCanChangeCountAttribute>()
                   ) }
             )

           // built-in type collections
           else if value :? seq<obj> then
             (value :?> seq<obj>)
             |> Seq.collecti (fun (i, collectionValue) ->
               let index = string i

               if
                 isNull collectionValue || collectionValue :? Def
                 || not (GenTypes.IsCustomType(collectionValue.GetType()))
               then
                 Seq.empty
               else
                 let indexHandle =
                   TranslationHandleUtility.GetBestHandleWithIndexForListElement(value, collectionValue)
                   |> Option.ofObj
                   |> Option.defaultValue index

                 collectInDefRecursive (
                   collectionValue,
                   newNormPath + "." + index,
                   newSuggestedPath + "." + indexHandle,
                   def,
                   visited,
                   acc
                 )
                 |> snd)

           // custom type collection
           else if
             not (isNull value)
             && GenTypes.IsCustomType(value.GetType())
           then
             collectInDefRecursive (value, newNormPath, newSuggestedPath, def, visited, acc)
             |> snd
           else
             Seq.empty)
     })


let private collectInDef (def: Def) =
  collectInDefRecursive (def, def.defName, def.defName, def, HashSet<obj>(), Seq.empty)
  |> snd


/// Collects all defInjectables of Def subtype from given mod.
///
/// defType is a Type of the Def subtype. modData is the mod's ModMetaData
let collectAllFor (defType: Type) (modData: ModMetaData) =
  GenDefDatabase.GetAllDefsInDatabaseForDef defType
  |> Seq.filter (fun def ->
    not (isNull def.modContentPack)
    && (def.modContentPack.PackageId = modData.PackageId))
  |> Seq.collect collectInDef
  |> List.ofSeq
