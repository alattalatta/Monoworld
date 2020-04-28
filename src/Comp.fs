module Infusion.Comp

open System
open System.Collections.Generic
open System.Text

open RimWorld
open Verse

open DefFields
open DefTool
open Lib
open StatMod
open VerseInterop

// Holds an equipment's infusions.
[<AllowNullLiteral>]
type Infusion() =
    inherit ThingComp()

    let mutable infusions = Set.empty<InfusionDef>

    let mutable quality = QualityCategory.Normal

    let mutable bestInfusionCache = None
    let mutable inspectStringCache = None

    let infusionsStatModCache = Dictionary<StatDef, option<StatMod>>()

    member this.Quality
        with get () = quality
        and set value = do quality <- value

    member this.Infusions
        with get () = infusions |> Seq.sortByDescending (fun inf -> inf.tier)
        and set (value: seq<InfusionDef>) =
            do this.InvalidateCache()
            infusions <- Seq.truncate (Settings.getBaseSlotsFor quality) value |> Set.ofSeq

    member this.InfusionsRaw = infusions

    member this.InfusionsByPosition =
        let (prefixes, suffixes) =
            this.Infusions
            |> Seq.fold (fun (pre, suf) cur ->
                if cur.position = Position.Prefix then (cur :: pre, suf) else (pre, cur :: suf))
                   (List.empty, List.empty)
        (List.rev prefixes, List.rev suffixes)

    member this.BestInfusion =
        match bestInfusionCache with
        | None ->
            do bestInfusionCache <- this.Infusions |> Seq.tryHead
            bestInfusionCache
        | _ -> bestInfusionCache

    member this.BestInfusionLabel =
        match this.BestInfusion with
        | Some bestInf ->
            let sb = StringBuilder(bestInf.label)
            if this.Size > 1 then do sb.Append("(+").Append(this.Size - 1).Append(")") |> ignore

            string sb
        | None -> ""

    member this.BestInfusionLabelShort =
        match this.BestInfusion with
        | Some bestInf ->
            let sb =
                StringBuilder(if bestInf.labelShort.NullOrEmpty() then bestInf.label else bestInf.labelShort)
            if this.Size > 1 then do sb.Append("(+").Append(this.Size - 1).Append(")") |> ignore

            string sb
        | None -> ""

    member this.InspectionLabel =
        if Set.isEmpty infusions then
            ""
        else
            let (prefixes, suffixes) = this.InfusionsByPosition

            let suffixedPart =
                if List.isEmpty suffixes then
                    this.parent.def.label
                else
                    let suffixString = (suffixes |> List.map (fun def -> def.label)).ToCommaList(true)
                    string (translate2 "Infusion.Label.Suffixed" suffixString this.parent.def.label)

            let prefixedPart =
                if List.isEmpty prefixes then
                    suffixedPart
                else
                    let prefixString =
                        prefixes
                        |> List.map (fun def -> def.label)
                        |> String.concat " "
                    string (translate2 "Infusion.Label.Prefixed" prefixString suffixedPart)

            prefixedPart.CapitalizeFirst()

    member this.Descriptions =
        this.Infusions
        |> Seq.map (fun inf -> inf.GetDescriptionString())
        |> Seq.fold (fun (acc: StringBuilder) cur -> acc.Append(cur)) (StringBuilder())
        |> string

    member this.Size = Set.count infusions

    member this.PopulateInfusionsStatModCache(stat: StatDef) =
        if not (infusionsStatModCache.ContainsKey stat) then
            let elligibles =
                infusions
                |> Seq.filter (fun inf -> inf.stats.ContainsKey stat)
                |> Seq.map (fun inf -> inf.stats.TryGetValue stat)

            let statMod =
                if Seq.isEmpty elligibles then
                    None
                else
                    elligibles
                    |> Seq.fold (+) StatMod.empty
                    |> Some

            do infusionsStatModCache.Add(stat, statMod)

    member this.GetModForStat(stat: StatDef) =
        do this.PopulateInfusionsStatModCache(stat)
        infusionsStatModCache.TryGetValue(stat, None) |> Option.defaultValue StatMod.empty

    member this.HasInfusionForStat(stat: StatDef) =
        do this.PopulateInfusionsStatModCache(stat)
        infusionsStatModCache.TryGetValue(stat, None) |> Option.isSome

    member this.InvalidateCache() =
        do bestInfusionCache <- None
        do inspectStringCache <- None
        do infusionsStatModCache.Clear()

    override this.TransformLabel label =
        match this.BestInfusion with
        | Some bestInf ->
            let parent = this.parent
            let baseLabel = GenLabel.ThingLabel(parent.def, parent.Stuff)

            let sb =
                match bestInf.position with
                | Position.Prefix -> translate2 "Infusion.Label.Prefixed" this.BestInfusionLabel baseLabel
                | Position.Suffix -> translate2 "Infusion.Label.Suffixed" this.BestInfusionLabel baseLabel
                | _ -> raise (ArgumentException("Position must be either Prefix or Suffix"))
                |> string
                |> StringBuilder

            // components
            // quality should never be None but let's be cautious
            let quality = compOfThing<CompQuality> parent |> Option.map (fun cq -> cq.Quality.GetLabel())

            let hitPoints =
                if parent.def.useHitPoints && parent.HitPoints < parent.MaxHitPoints && parent.def.stackLimit = 1
                then Some((float32 parent.HitPoints / float32 parent.MaxHitPoints).ToStringPercent())
                else None

            let tainted =
                match parent with
                | :? Apparel as apparel ->
                    if apparel.WornByCorpse then Some(translate "WornByCorpseChar") else None
                | _ -> None

            do [ quality; hitPoints; tainted ]
               |> List.choose id
               |> String.concat " "
               |> (fun str -> if not (str.NullOrEmpty()) then sb.Append(" (").Append(str).Append(")") |> ignore)

            string sb
        | None -> label

    override this.GetDescriptionPart() = this.Descriptions

    override this.DrawGUIOverlay() =
        if Find.CameraDriver.CurrentZoom <= CameraZoomRange.Close then
            match this.BestInfusion with
            | Some bestInf ->
                do GenMapUI.DrawThingLabel
                    (GenMapUI.LabelDrawPosFor(this.parent, -0.6499999762f), this.BestInfusionLabelShort,
                     tierToColor bestInf.tier)
            | None -> ()

    override this.PostExposeData() =
        let mutable savedQuality = QualityCategory.Normal
        if (Scribe.mode = LoadSaveMode.LoadingVars) then
            do Scribe_Values.Look(&savedQuality, "quality")

            do this.Quality <- savedQuality

        let mutable savedData = ResizeArray infusions
        if (Scribe.mode = LoadSaveMode.LoadingVars || (Scribe.mode = LoadSaveMode.Saving && savedData.Any())) then
            do Scribe_Collections.Look(&savedData, "infusion", LookMode.Def)

            if not (isNull savedData) && Scribe.mode = LoadSaveMode.LoadingVars then do this.Infusions <- savedData

    override this.AllowStackWith(_) = false

let addInfusion (infDef: InfusionDef) (comp: Infusion) =
    do comp.Infusions <-
        seq {
            yield infDef
            yield! comp.Infusions
        }

/// Picks elligible `InfusionDef` for the `Thing`.
let pickInfusions (quality: QualityCategory) (parent: ThingWithComps) =
    // requirement fields
    let checkAllowance (infDef: InfusionDef) =
        if parent.def.IsApparel then infDef.requirements.allowance.apparel
        elif parent.def.IsMeleeWeapon then infDef.requirements.allowance.melee
        elif parent.def.IsRangedWeapon then infDef.requirements.allowance.ranged
        else false

    let checkDisabled (infDef: InfusionDef) = not infDef.disabled
    let checkTechLevel (infDef: InfusionDef) = infDef.requirements.techLevel |> Seq.contains parent.def.techLevel
    let checkQuality (infDef: InfusionDef) = (infDef.ChanceFor quality) > 0.0f

    let checkDamageType (infDef: InfusionDef) =
        if parent.def.IsApparel || infDef.requirements.meleeDamageType = DamageType.Anything then
            true
        else
            parent.def.tools
            |> Seq.reduce (fun a b ->
                if a.power > b.power then a else b)
            |> isToolCapableOfDamageType infDef.requirements.meleeDamageType

    // chance
    let checkChance (infDef: InfusionDef) =
        let chance = infDef.ChanceFor(quality) * Settings.getChanceFactor()
        Rand.Chance chance

    DefDatabase<InfusionDef>.AllDefs
    |> Seq.filter (checkDisabled <&> checkAllowance <&> checkTechLevel <&> checkQuality <&> checkDamageType)
    |> Seq.map (fun infDef -> (infDef, (infDef.WeightFor quality) * (Settings.getWeightFactor()) + Rand.Value)) // weighted, duh
    |> Seq.sortByDescending snd
    |> Seq.truncate (Settings.getBaseSlotsFor quality)
    |> Seq.map fst
    |> Seq.filter checkChance
    |> List.ofSeq // need to "finalize" the random sort
    |> List.sortBy (fun infDef -> infDef.tier)

let removeAllInfusions (comp: Infusion) = do comp.Infusions <- Set.empty

let removeInfusion (infDef: InfusionDef) (comp: Infusion) = do comp.Infusions <- Set.remove infDef comp.InfusionsRaw

let resetHP (comp: ThingComp) = do comp.parent.HitPoints <- comp.parent.MaxHitPoints
