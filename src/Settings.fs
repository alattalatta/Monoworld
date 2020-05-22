module Infusion.Settings

open System

open HugsLib
open HugsLib.Settings
open RimWorld
open Verse

open Lib
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


module ListSettingMaker =
    type handleInfoSet<'a> =
        { key: string
          label: string
          desc: string
          defaultValue: 'a
          validator: Settings.SettingHandle.ValueIsValid }

    let make<'a, 'b when 'a: comparison> (infoOf: 'a -> handleInfoSet<'b>) uniq keys =
        let mutable settingsOpened = false

        let makeHandle (pack: ModSettingsPack) (a: 'a) =
            let { key = key; label = label; desc = desc; defaultValue = defaultValue } = infoOf a

            let handle =
                pack.GetHandle(key, label, desc, defaultValue)
            // bonus point for "<- fun () ->"
            do handle.VisibilityPredicate <- fun () -> settingsOpened

            (a, handle)

        let populate pack =
            keys |> List.map (makeHandle pack) |> Map.ofList

        let draw (pack: ModSettingsPack) =
            let slotSettingOpener =
                pack.GetHandle("slotsOpened_" + uniq, "", translate "", false)

            do slotSettingOpener.CustomDrawer <-
                (fun rect ->
                    let buttonLabel =
                        if settingsOpened
                        then sprintf "Infusion.Settings.%s.CloseSettings" uniq
                        else sprintf "Infusion.Settings.%s.OpenSettings" uniq

                    let clicked =
                        Widgets.ButtonText(rect, translate buttonLabel)

                    if clicked then do settingsOpened <- not settingsOpened
                    // nothing is really being changed, just return false
                    false)

            pack

        (populate, draw)


module Slots =
    open ListSettingMaker

    let mutable handles = Map.empty

    let keys =
        [ QualityCategory.Normal
          QualityCategory.Good
          QualityCategory.Excellent
          QualityCategory.Masterwork
          QualityCategory.Legendary ]

    let defaults = [| 1; 1; 2; 3; 4 |]

    let makeInfoSet (defaultValue: int) (quality: QualityCategory) =
        let qualityName =
            Enum.GetName(typeof<QualityCategory>, quality)

        let label =
            translate (sprintf "QualityCategory_%s" qualityName)
            |> GenText.CapitalizeFirst

        { key = "slots" + qualityName
          label = label
          desc = string (translate1 "Infusion.Settings.Slot.Description" defaultValue)
          defaultValue = defaults.[int (quality - QualityCategory.Normal)]
          validator = Validators.IntRangeValidator(0, 20) }

    let draw pack =
        let (populate, draw) =
            make<QualityCategory, int> (makeInfoSet 3) "Slots" keys

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


let initialize () =
    HugsLibController.SettingsManager.GetModSettings("latta.infusion")
    |> AccuracyOvercap.draw
    |> SelectionConsts.draw
    |> SlotModifiers.draw
    |> Slots.draw
    |> Tiers.draw
    |> ignore
