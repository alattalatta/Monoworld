module Infusion.Settings

open System

open HugsLib
open HugsLib.Settings
open Poet.Lib
open Poet.Lyric.Translation
open RimWorld
open UnityEngine
open Verse

/// Returns translated label and description of the given setting key.
let getTranslatedStrings (key: string) =
  let labelKey =
    sprintf "Infusion.Settings.%s" (key.CapitalizeFirst())

  let descKey = sprintf "%s.Description" labelKey

  (translate labelKey, translate descKey)

let getHandle (defaultValue: 't) (key: string) (pack: ModSettingsPack) =
  let (label, desc) = getTranslatedStrings key

  pack.GetHandle(key, label, desc, defaultValue)

/// Validatable variant of `getHandle`.
let getValidatingHandle
  (defaultValue: 't)
  (key: string)
  (validator: SettingHandle.ValueIsValid)
  (pack: ModSettingsPack)
  =
  let (label, desc) = getTranslatedStrings key

  pack.GetHandle(key, label, desc, defaultValue, validator)


module AccuracyOvercap =
  let mutable handle: SettingHandle<bool> = null

  let draw (pack: ModSettingsPack) =
    do handle <- getHandle true "accuracyOvercapping" pack

    pack


module BiocodeBonus =
  let mutable handle: SettingHandle<bool> = null

  let draw (pack: ModSettingsPack) =
    do handle <- getHandle true "biocodeBonus" pack

    pack


module ExtractionChanceFactor =
  let mutable handle: SettingHandle<float32> = null

  let draw (pack: ModSettingsPack) =
    do handle <- getValidatingHandle 1.0f "extractionChanceFactor" (Validators.FloatRangeValidator(0.0f, 100.0f)) pack

    pack


module ReusableInfusers =
  let mutable handle: SettingHandle<bool> = null

  let draw (pack: ModSettingsPack) =
    do handle <- getHandle false "reusableInfusers" pack

    pack


module SelectionConsts =
  let mutable chanceHandle: SettingHandle<float32> = null
  let mutable muHandle: SettingHandle<float32> = null
  let mutable sigmaHandle: SettingHandle<float32> = null

  let draw (pack: ModSettingsPack) =
    do chanceHandle <- getValidatingHandle 1.0f "chanceFactor" (Validators.FloatRangeValidator(0.0f, 100.0f)) pack
    do muHandle <- getValidatingHandle 1.0f "mu" (Validators.FloatRangeValidator(0.5f, 10.0f)) pack
    do sigmaHandle <- getValidatingHandle 1.5f "sigma" (Validators.FloatRangeValidator(0.5f, 10.0f)) pack

    pack


module SlotModifiers =
  let mutable bodyPartHandle: SettingHandle<bool> = null
  let mutable layerHandle: SettingHandle<bool> = null

  let draw (pack: ModSettingsPack) =
    do bodyPartHandle <- getHandle true "bodyPartLimit" pack
    do layerHandle <- getHandle true "layerBonuses" pack

    pack


module ListSettingMaker =
  type HandleInfoSet<'b> =
    { key: string
      label: string
      desc: string
      defaultValue: 'b
      validator: Settings.SettingHandle.ValueIsValid }

  let make<'a, 'b when 'a: comparison> (infoOf: 'a -> HandleInfoSet<'b>) uniq keys =
    let mutable settingsOpened = false

    let makeHandle (pack: ModSettingsPack) (a: 'a) =
      let { key = key
            label = label
            desc = desc
            defaultValue = defaultValue
            validator = validator } =
        infoOf a

      let handle =
        pack.GetHandle(key, label, desc, defaultValue)

      do handle.VisibilityPredicate <- fun () -> settingsOpened
      do handle.Validator <- validator

      (a, handle)

    let populate pack =
      keys |> List.map (makeHandle pack) |> Map.ofList

    let draw (pack: ModSettingsPack) =
      let slotSettingOpener =
        pack.GetHandle("slotsOpened_" + uniq, "", "", false)

      do slotSettingOpener.Unsaved <- true

      do
        slotSettingOpener.CustomDrawer <-
          (fun rect ->
            let buttonLabel =
              if settingsOpened then
                sprintf "Infusion.Settings.%s.CloseSettings" uniq
              else
                sprintf "Infusion.Settings.%s.OpenSettings" uniq

            let clicked =
              Widgets.ButtonText(rect, translate buttonLabel)

            if clicked then
              do settingsOpened <- not settingsOpened
            // nothing is really being changed, just return false
            false)

      pack

    (populate, draw)


module Slots =
  open ListSettingMaker

  let mutable handles = Map.empty

  let keys =
    [ QualityCategory.Awful
      QualityCategory.Poor
      QualityCategory.Normal
      QualityCategory.Good
      QualityCategory.Excellent
      QualityCategory.Masterwork
      QualityCategory.Legendary ]

  let defaults = [| 0; 0; 1; 1; 2; 2; 3 |]

  let makeInfoSet (quality: QualityCategory) =
    let qualityName =
      Enum.GetName(typeof<QualityCategory>, quality)

    let label =
      translate (sprintf "QualityCategory_%s" qualityName)
      |> GenText.CapitalizeFirst

    let defaultValue =
      defaults.[int (quality - QualityCategory.Awful)]

    { key = "slots" + qualityName
      label = label
      desc = string (translate1 "Infusion.Settings.Slot.Description" defaultValue)
      defaultValue = defaultValue
      validator = Validators.IntRangeValidator(0, 20) }

  let draw pack =
    let (populate, draw) =
      make<QualityCategory, int> makeInfoSet "Slots" keys

    do handles <- populate pack
    draw pack

  let getBaseSlotsFor quality =
    Map.tryFind quality handles
    |> Option.map (fun h -> h.Value)
    |> Option.defaultValue 0


module Tiers =
  open ListSettingMaker

  let mutable handles = Map.empty

  let keys =
    DefDatabase<TierDef>.AllDefsListForReading
    |> List.ofSeq

  let makeInfoSet (tier: TierDef) =
    let infusionsCount =
      DefDatabase<InfusionDef>.AllDefsListForReading
      |> Seq.filter (fun inf -> inf.tier = tier)
      |> Seq.length

    let label =
      sprintf "%s (%s)" tier.defName tier.label

    { key = "tier_" + tier.defName
      label = label
      desc = sprintf "%d defs found" infusionsCount
      defaultValue = true
      validator = null }

  let draw pack =
    let (populate, draw) =
      make<TierDef, bool> makeInfoSet "Tiers" keys

    do handles <- populate pack

    draw pack

  let isEnabled tier =
    Map.tryFind tier handles
    |> Option.map (fun t -> t.Value)
    |> Option.defaultValue false


module Divider =
  let draw key (pack: ModSettingsPack) =
    let divider =
      pack.GetHandle("__divider__" + key, "", "", false)

    do divider.Unsaved <- true

    do
      divider.CustomDrawer <-
        (fun rect ->
          do GUI.color <- Color(1.0f, 1.0f, 1.0f, 0.7f)
          do Widgets.DrawLineHorizontal(rect.xMin, rect.yMin + (rect.height / 2.0f), rect.width)
          do GUI.color <- Color.white
          false)

    pack


let initialize () =
  HugsLibController.SettingsManager.GetModSettings("latta.infusion")
  |> AccuracyOvercap.draw
  |> BiocodeBonus.draw
  |> ExtractionChanceFactor.draw
  |> ReusableInfusers.draw
  |> SelectionConsts.draw
  |> SlotModifiers.draw
  |> Divider.draw "1"
  |> Slots.draw
  |> Divider.draw "2"
  |> Tiers.draw
  |> ignore
