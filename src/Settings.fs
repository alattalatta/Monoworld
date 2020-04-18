module Infusion.Settings

open HugsLib
open HugsLib.Settings

open VerseInterop

let mutable chanceFactor: option<SettingHandle<float32>> = None
let mutable weightFactor: option<SettingHandle<float32>> = None

let getChanceFactor() = Option.map float32 chanceFactor |> Option.defaultValue 1.0f
let getWeightFactor() = Option.map float32 weightFactor |> Option.defaultValue 0.5f

let initialize() =
    let pack = HugsLibController.SettingsManager.GetModSettings("latta.infusion")

    do chanceFactor <-
        Some
            (pack.GetHandle
                ("chanceFactor", (translate "Infusion.Settings.ChanceFactor"),
                 (translate "Infusion.Settings.ChanceFactor.Description"), 1.0f,
                 Validators.FloatRangeValidator(0.0f, 100.0f)))
    do weightFactor <-
        Some
            (pack.GetHandle
                ("weightFactor", (translate "Infusion.Settings.WeightFactor"),
                 (translate "Infusion.Settings.WeightFactor.Description"), 0.5f,
                 Validators.FloatRangeValidator(0.0f, 2.0f)))
