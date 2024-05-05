module ANDH.Settings

open HugsLib
open HugsLib.Settings
open Poet.Lyric.Translation
open Verse


/// Returns translated label and description of the given setting key.
let private getTranslatedStrings (key: string) =
  let labelKey = sprintf "ANDH.%s" (key.CapitalizeFirst())

  let descKey = sprintf "%s.Description" labelKey

  (translate labelKey, translate descKey)


let private getHandle (defaultValue: 't) (key: string) (pack: ModSettingsPack) =
  let (label, desc) = getTranslatedStrings key

  pack.GetHandle(key, label, desc, defaultValue)


/// Validatable variant of `getHandle`.
let private getValidatingHandle
  (defaultValue: 't)
  (key: string)
  (validator: SettingHandle.ValueIsValid)
  (pack: ModSettingsPack)
  =
  let (label, desc) = getTranslatedStrings key

  pack.GetHandle(key, label, desc, defaultValue, validator)


module BondedDetectionChance =
  let mutable handle: SettingHandle<float32> = null

  let draw (pack: ModSettingsPack) =
    handle <- getValidatingHandle 0.60f "bondedDetectionChance" (Validators.FloatRangeValidator(0.0f, 1.0f)) pack
    pack


module CanTriggerEmerging =
  let mutable handle: SettingHandle<bool> = null

  let draw (pack: ModSettingsPack) =
    handle <- getHandle true "canTriggerEmerging" pack
    pack


module DetectionChance =
  let mutable handle: SettingHandle<float32> = null

  let draw (pack: ModSettingsPack) =
    handle <- getValidatingHandle 0.25f "detectionChance" (Validators.FloatRangeValidator(0.0f, 1.0f)) pack
    pack
    

module DetectionRadius =
  let mutable handle: SettingHandle<float32> = null

  let draw (pack: ModSettingsPack) =
    handle <- getValidatingHandle 20.0f "detectionRadius" (Validators.FloatRangeValidator(3.0f, 100.0f)) pack
    pack
    

module EmergenceChance =
  let mutable handle: SettingHandle<float32> = null

  let draw (pack: ModSettingsPack) =
    handle <- getValidatingHandle 0.1f "emergenceChance" (Validators.FloatRangeValidator(0.0f, 1.0f)) pack
    pack


module StressedFalseAlarm =
  let mutable handle: SettingHandle<float32> = null

  let draw (pack: ModSettingsPack) =
    handle <- getValidatingHandle 0.01f "stressedFalseAlarm" (Validators.FloatRangeValidator(0.0f, 1.0f)) pack
    pack


let initialize () =
  HugsLibController.SettingsManager.GetModSettings("latta.andh")
  |> DetectionChance.draw
  |> BondedDetectionChance.draw
  |> DetectionRadius.draw
  |> CanTriggerEmerging.draw
  |> EmergenceChance.draw
  |> StressedFalseAlarm.draw
  |> ignore
