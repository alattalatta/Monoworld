module Infusion.Settings

open System

open HugsLib
open HugsLib.Settings
open RimWorld
open Verse

open VerseInterop

module AccuracyOvercap =
    let mutable handle: SettingHandle<bool> = null

    let draw (pack: ModSettingsPack) =
        do handle <-
            (pack.GetHandle
                ("accuracyOvercapping",
                 (translate "Infusion.Settings.AccuracyOvercapping"),
                 (translate "Infusion.Settings.AccuracyOvercapping.Description"),
                 true))
        pack

module SelectionConsts =
    let mutable chanceHandle: SettingHandle<float32> = null
    let mutable weightHandle: SettingHandle<float32> = null

    let draw (pack: ModSettingsPack) =
        do chanceHandle <-
            (pack.GetHandle
                ("chanceFactor",
                 (translate "Infusion.Settings.ChanceFactor"),
                 (translate "Infusion.Settings.ChanceFactor.Description"),
                 1.0f,
                 Validators.FloatRangeValidator(0.0f, 100.0f)))
        do weightHandle <-
            (pack.GetHandle
                ("weightFactor",
                 (translate "Infusion.Settings.WeightFactor"),
                 (translate "Infusion.Settings.WeightFactor.Description"),
                 1.0f,
                 Validators.FloatRangeValidator(0.0f, 2.0f)))
        pack

module SlotModifiers =
    let mutable bodyPartHandle: SettingHandle<bool> = null
    let mutable layerHandle: SettingHandle<bool> = null

    let draw (pack: ModSettingsPack) =
        do bodyPartHandle <-
            (pack.GetHandle
                ("bodyPartLimit",
                 (translate "Infusion.Settings.BodyPartLimit"),
                 (translate "Infusion.Settings.BodyPartLimit.Description"),
                 true))
        do layerHandle <-
            (pack.GetHandle
                ("layerBonuses",
                 (translate "Infusion.Settings.LayerBonuses"),
                 (translate "Infusion.Settings.LayerBonuses.Description"),
                 true))
        pack

module Slots =
    let mutable handles: Map<QualityCategory, SettingHandle<int>> = Map.empty
    let mutable settingsOpened = false

    let getBaseSlotsFor (quality: QualityCategory) =
        Map.tryFind quality handles
        |> Option.map int
        |> Option.defaultValue 0

    let private slotSettingHandle (quality: QualityCategory) defaultValue (pack: ModSettingsPack) =
        let qualityName =
            Enum.GetName(typeof<QualityCategory>, quality)

        let handle =
            pack.GetHandle
                (sprintf "slots%s" qualityName,
                 (translate (sprintf "QualityCategory_%s" qualityName)).CapitalizeFirst(),
                 string (translate1 "Infusion.Settings.Slot" defaultValue),
                 defaultValue,
                 Validators.IntRangeValidator(0, 20))
        // bonus point for "<- fun () ->"
        do handle.VisibilityPredicate <- fun () -> settingsOpened

        (quality, handle)

    let draw (pack: ModSettingsPack) =
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
                // nothing is really being changed, just return false
                false)

        do handles <-
            [ slotSettingHandle QualityCategory.Normal 1
              slotSettingHandle QualityCategory.Good 1
              slotSettingHandle QualityCategory.Excellent 2
              slotSettingHandle QualityCategory.Masterwork 3
              slotSettingHandle QualityCategory.Legendary 4 ]
            |> List.map (fun f -> f pack) // consider it as a reversed mapping
            |> Map.ofList
        pack

let initialize () =
    HugsLibController.SettingsManager.GetModSettings("latta.infusion")
    |> AccuracyOvercap.draw
    |> SelectionConsts.draw
    |> SlotModifiers.draw
    |> Slots.draw
    |> ignore
