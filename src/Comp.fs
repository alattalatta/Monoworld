module Infusion.Comp

open System
open System.Text

open RimWorld
open Verse

open DefFields
open DefTool
open Lib
open VerseInterop

// Holds an equipment's infusions.
[<AllowNullLiteral>]
type Infusion() =
    inherit ThingComp()

    let mutable infusions = Set.empty<InfusionDef>

    let mutable bestInfusionCache = None
    let mutable inspectStringCache = None

    let mutable quality = QualityCategory.Normal

    member this.Quality
        with get () = quality
        and set value = do quality <- value

    member this.Infusions
        with get () = infusions |> Seq.sortByDescending (fun inf -> inf.tier)
        and set (value: seq<InfusionDef>) =
            do this.InvalidateCache()
            infusions <- Seq.truncate (maxInfusionsFor quality) value |> Set.ofSeq

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

    member this.LabelFull =
        let pickLabel (def: InfusionDef) = def.label

        if Set.isEmpty infusions then
            ""
        else
            let (prefixes, suffixes) = this.InfusionsByPosition
            let sb = StringBuilder()

            if not (List.isEmpty prefixes) then
                do (prefixes
                    |> List.map pickLabel
                    |> String.concat " "
                    |> sb.Append).Append(" ")
                   |> ignore

            do sb.Append(this.parent.def.label) |> ignore

            if not (List.isEmpty suffixes) then
                let infusions = (suffixes |> List.map pickLabel).ToCommaList(true)
                do sb.Append(" ").Append(translate "OfLower").Append(" ").Append(infusions) |> ignore

            string sb |> GenText.CapitalizeFirst

    member this.Descriptions =
        this.Infusions
        |> Seq.map (fun inf -> inf.GetDescriptionString())
        |> Seq.fold (fun (acc: StringBuilder) cur -> acc.Append(cur)) (StringBuilder())
        |> string

    member this.Size = Set.count infusions

    member this.HasInfusionForStat(stat: StatDef) = infusions |> Set.exists (fun inf -> inf.stats.ContainsKey stat)

    member this.InvalidateCache() =
        do bestInfusionCache <- None
        do inspectStringCache <- None

    override this.TransformLabel label =
        match this.BestInfusion with
        | Some bestInf ->
            let parent = this.parent
            let baseLabel = GenLabel.ThingLabel(parent.def, parent.Stuff)

            let sb =
                match bestInf.position with
                | Position.Prefix -> sprintf "%s %s" this.BestInfusionLabel baseLabel
                | Position.Suffix -> sprintf "%s %s %s" baseLabel (translate "OfLower") this.BestInfusionLabel
                | _ -> raise (ArgumentException("Position must be one either Prefix or Suffix"))
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

    override this.CompInspectStringExtra() =
        match inspectStringCache with
        | Some cache -> cache
        | None ->
            let inspectString = this.LabelFull
            do inspectStringCache <- Some inspectString
            inspectString

    override this.DrawGUIOverlay() =
        if Find.CameraDriver.CurrentZoom <= CameraZoomRange.Close then
            match this.BestInfusion with
            | Some bestInf ->
                do GenMapUI.DrawThingLabel
                    (GenMapUI.LabelDrawPosFor(this.parent, -0.6499999762f), this.BestInfusionLabelShort,
                     tierToColor bestInf.tier)
            | None -> ()

    override this.PostExposeData() =
        let mutable savedData = ResizeArray infusions

        if (Scribe.mode = LoadSaveMode.LoadingVars || (Scribe.mode = LoadSaveMode.Saving && savedData.Any())) then
            do Scribe_Collections.Look(&savedData, "infusion", LookMode.Def)

            if not (isNull savedData) && Scribe.mode = LoadSaveMode.LoadingVars then do this.Infusions <- set savedData

    override this.AllowStackWith(_) = false

let addInfusion (infDef: InfusionDef) (comp: Infusion) =
    do comp.Infusions <-
        seq {
            yield infDef
            yield! comp.Infusions
        }

let allModsForStat (stat: StatDef) (comp: Infusion) =
    comp.Infusions
    |> Seq.filter (fun inf -> inf.stats.ContainsKey stat)
    |> Seq.map (fun inf -> inf.stats.TryGetValue stat)
    |> List.ofSeq

let removeAllInfusions (comp: Infusion) = do comp.Infusions <- Set.empty

let removeInfusion (infDef: InfusionDef) (comp: Infusion) = do comp.Infusions <- Set.remove infDef comp.InfusionsRaw

let resetHP (comp: ThingComp) = do comp.parent.HitPoints <- comp.parent.MaxHitPoints

/// Picks elligible `InfusionDef` for the `Thing`.
let pickInfusions (quality: QualityCategory) (parent: ThingWithComps) =
    // requirement fields
    let checkAllowance (infDef: InfusionDef) =
        if parent.def.IsApparel then infDef.requirements.allowance.apparel
        elif parent.def.IsMeleeWeapon then infDef.requirements.allowance.melee
        elif parent.def.IsRangedWeapon then infDef.requirements.allowance.ranged
        else false

    let checkTechLevel (infDef: InfusionDef) = infDef.requirements.techLevel |> Seq.contains parent.def.techLevel
    let checkQuality (infDef: InfusionDef) = (infDef.ChanceFor quality) > 0.0f

    // chance
    let checkChance (infDef: InfusionDef) =
        let chance = infDef.ChanceFor(quality)
        Rand.Chance chance

    DefDatabase<InfusionDef>.AllDefs
    |> Seq.filter (checkAllowance <&> checkTechLevel <&> checkQuality)
    |> Seq.map (fun infDef -> (infDef, (infDef.WeightFor quality * 1.2f) + Rand.Value)) // weighted, duh
    |> Seq.sortByDescending snd
    |> Seq.truncate (maxInfusionsFor quality)
    |> Seq.map fst
    |> Seq.filter checkChance
    |> List.ofSeq // need to "finalize" the random sort
    |> List.sortBy (fun infDef -> infDef.tier)
