module DevL10N.TranslationOutput.Injectable

open System
open System.Reflection

open Verse


type Injectable =
  { CurValue: string
    CurValueCollection: string list
    Def: Def
    FieldInfo: FieldInfo
    IsCollection: bool
    ListInjectionAllowed: bool
    NormalizedPath: string
    SuggestedPath: string }


/// Collects all defInjectables of Def subtype from given mod.
///
/// defType is a Type of the Def subtype. modData is the mod's ModMetaData
let collectAllFor (defType: Type) (modData: ModMetaData) =
  let possibleDefInjections = ResizeArray<Injectable>()

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

  let traverser = DefInjectionUtility.PossibleDefInjectionTraverser(traverserBody)

  do DefInjectionUtility.ForEachPossibleDefInjection(defType, traverser, modData)

  possibleDefInjections |> List.ofSeq
