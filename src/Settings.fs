module Infusion.Settings

open System

open HugsLib
open HugsLib.Settings
open RimWorld
open Verse

open VerseInterop

// [todo] separate

// accuracy overcapping
let mutable accuracyOvercapEnabled: SettingHandle<bool> = null

let getAccuracyOvercapEnabled () = accuracyOvercapEnabled.Value

// choice factors
let mutable chanceFactor: SettingHandle<float32> = null
let mutable weightFactor: SettingHandle<float32> = null

let getChanceFactor () = chanceFactor.Value
let getWeightFactor () = weightFactor.Value

// slots
let mutable slots: Map<QualityCategory, SettingHandle<int>> = Map.empty

let getBaseSlotsFor (quality: QualityCategory) =
    Map.tryFind quality slots
    |> Option.map int
    |> Option.defaultValue 1

// internal states
let mutable settingsOpened = false

let initialize () =
    let pack =
        HugsLibController.SettingsManager.GetModSettings("latta.infusion")

    // accuracy overcapping
    do accuracyOvercapEnabled <-
        (pack.GetHandle
            ("accuracyOvercapping",
             (translate "Infusion.Settings.AccuracyOvercapping"),
             (translate "Infusion.Settings.AccuracyOvercapping.Description"),
             true))

    // choice factors
    do chanceFactor <-
        (pack.GetHandle
            ("chanceFactor",
             (translate "Infusion.Settings.ChanceFactor"),
             (translate "Infusion.Settings.ChanceFactor.Description"),
             1.0f,
             Validators.FloatRangeValidator(0.0f, 100.0f)))
    do weightFactor <-
        (pack.GetHandle
            ("weightFactor",
             (translate "Infusion.Settings.WeightFactor"),
             (translate "Infusion.Settings.WeightFactor.Description"),
             0.5f,
             Validators.FloatRangeValidator(0.0f, 2.0f)))

    // slots
    let slotSettingOpener =
        pack.GetHandle("slotsOpened", "", translate "", false)

    do slotSettingOpener.CustomDrawer <-
        (fun rect ->
            let buttonLabel =
                if settingsOpened
                then "Infusion.Settings.Slots.CloseSlotSettings"
                else "Infusion.Settings.Slots.OpenSlotSettings"

            let clicked =
                Widgets.ButtonText(rect, translate buttonLabel)

            if clicked then do settingsOpened <- not settingsOpened
            false)

    let slotSettingHandle (quality: QualityCategory) defaultValue =
        let qualityName =
            Enum.GetName(typeof<QualityCategory>, quality)

        let handle =
            pack.GetHandle
                (sprintf "slots%s" qualityName,
                 (translate (sprintf "QualityCategory_%s" qualityName)).CapitalizeFirst(),
                 "0 ~ 20",
                 defaultValue,
                 Validators.IntRangeValidator(0, 20))
        // bonus point for "<- fun () ->"
        do handle.VisibilityPredicate <- fun () -> settingsOpened

        (quality, handle)

    do slots <-
        slots.Add(slotSettingHandle QualityCategory.Normal 1).Add(slotSettingHandle QualityCategory.Good 1)
             .Add(slotSettingHandle QualityCategory.Excellent 2).Add(slotSettingHandle QualityCategory.Masterwork 3)
             .Add(slotSettingHandle QualityCategory.Legendary 4)
