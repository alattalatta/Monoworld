namespace Infusion

open System
open System.Collections.Generic
open System.Text

open Poet.Lib
open Poet.Lyric
open Poet.Lyric.Translation
open RimWorld
open Verse

open DefFields
open StatMod

type BestInfusionLabelLength =
    | Long
    | Short


// Holds an equipment's infusions.
[<AllowNullLiteral>]
type CompInfusion() =
    inherit ThingComp()

    static let mutable wantingCandidates = Set.empty<CompInfusion>
    static let mutable extractionCandidates = Set.empty<CompInfusion>
    static let mutable removalCandidates = Set.empty<CompInfusion>

    let mutable infusions = Set.empty<InfusionDef>
    let mutable wantingSet = Set.empty<InfusionDef>
    let mutable extractionSet = Set.empty<InfusionDef>
    let mutable removalSet = Set.empty<InfusionDef>

    let mutable biocoder = None
    let mutable effectsEnabled = true
    let mutable slotCount = -1
    let mutable quality = QualityCategory.Normal

    let mutable bestInfusionCache = None
    let mutable onHitsCache = None
    let infusionsStatModCache = Dictionary<StatDef, option<StatMod>>()

    static member WantingCandidates
        with get () = wantingCandidates
        and set value = do wantingCandidates <- value

    static member ExtractionCandidates
        with get () = extractionCandidates
        and set value = do extractionCandidates <- value

    static member RemovalCandidates
        with get () = removalCandidates
        and set value = do removalCandidates <- value

    static member RegisterWantingCandidate comp =
        do CompInfusion.WantingCandidates <- Set.add comp CompInfusion.WantingCandidates

    static member RegisterExtractionCandidate comp =
        do CompInfusion.ExtractionCandidates <- Set.add comp CompInfusion.ExtractionCandidates

    static member RegisterRemovalCandidate comp =
        do CompInfusion.RemovalCandidates <- Set.add comp CompInfusion.RemovalCandidates

    static member UnregisterWantingCandidates comp =
        do CompInfusion.WantingCandidates <- Set.remove comp CompInfusion.WantingCandidates

    static member UnregisterExtractionCandidates comp =
        do CompInfusion.ExtractionCandidates <- Set.remove comp CompInfusion.ExtractionCandidates

    static member UnregisterRemovalCandidate comp =
        do CompInfusion.RemovalCandidates <- Set.remove comp CompInfusion.RemovalCandidates

    member this.Biocoder
        with get () = biocoder
        and set (value: CompBiocodable option) = do biocoder <- value

    member this.EffectsEnabled = effectsEnabled

    member this.Quality
        with get () = quality
        and set value =
            do quality <- value
               slotCount <- this.CalculateSlotCountFor value

    member this.Infusions
        with get () = infusions |> Seq.sortDescending
        and set (value: seq<InfusionDef>) =
            let hitPointsRatio =
                float32 this.parent.HitPoints
                / float32 this.parent.MaxHitPoints

            do infusions <- value |> Set.ofSeq
               wantingSet <- Set.difference wantingSet infusions
               extractionSet <- Set.intersect extractionSet infusions
               removalSet <- Set.intersect removalSet infusions

               this.InvalidateCache()
               this.FinalizeSetMutations()

               this.parent.HitPoints <-
                   (float32 this.parent.MaxHitPoints * hitPointsRatio)
                   |> round
                   |> int
                   |> min this.parent.MaxHitPoints


    member this.InfusionsRaw = infusions

    member this.WantingSet
        with get () = wantingSet
        and set value =
            do wantingSet <- value

            this.FinalizeSetMutations()

    member this.FirstWanting = Seq.tryHead wantingSet

    member this.ExtractionSet
        with get () = extractionSet
        and set value =
            do extractionSet <- value
            do removalSet <- Set.difference removalSet extractionSet

            this.FinalizeSetMutations()

    member this.FirstExtraction = Seq.tryHead extractionSet

    member this.RemovalSet
        with get () = removalSet
        and set value =
            do removalSet <- value
            do extractionSet <- Set.difference extractionSet removalSet

            this.FinalizeSetMutations()

    member this.SlotCount
        with get () = slotCount
        and set value = do slotCount <- value

    member this.InfusionsByPosition =
        let (prefixes, suffixes) =
            this.Infusions
            |> Seq.fold (fun (pre, suf) cur ->
                if cur.position = Position.Prefix then (cur :: pre, suf) else (pre, cur :: suf))
                   (List.empty, List.empty)

        (List.rev prefixes, List.rev suffixes)

    member this.BestInfusion =
        if Option.isNone bestInfusionCache then do this.InvalidateCache()

        bestInfusionCache

    member this.OnHits =
        if Option.isNone onHitsCache then do this.InvalidateCache()

        Option.defaultValue List.empty onHitsCache

    member this.Descriptions =
        this.Infusions
        |> Seq.map InfusionDef.makeDescriptionString
        |> String.concat "\n\n"

    // [todo] move to under ITab
    member this.InspectionLabel =
        if Set.isEmpty infusions then
            (translate1 "Infusion.Label.NotInfused" this.parent.def.label).CapitalizeFirst()
            |> string
        else
            let (prefixes, suffixes) = this.InfusionsByPosition

            let suffixedPart =
                if List.isEmpty suffixes then
                    this.parent.def.label
                else
                    let suffixString =
                        (suffixes |> List.map (fun def -> def.label)).ToCommaList(true)

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

    member this.Size = Set.count infusions

    // can't use CompGetGizmosExtra, Pawn_EquipmentTracker do not use them for Pawn Gizmos.
    member this.EffectGizmo =
        onHitsCache
        // List.length != 0
        |> Option.filter (List.length >> (=) 0 >> not)
        |> Option.map (fun _ ->
            Command_Toggle
                (defaultLabel = ResourceBank.Strings.Gizmo.label,
                 defaultDesc = ResourceBank.Strings.Gizmo.desc,
                 icon = ResourceBank.Textures.Flame,
                 isActive = (fun () -> effectsEnabled),
                 toggleAction = (fun () -> do effectsEnabled <- not effectsEnabled)))

    member this.PopulateInfusionsStatModCache(stat: StatDef) =
        if not (infusionsStatModCache.ContainsKey stat) then
            let elligibles =
                infusions
                |> Seq.filter (fun inf -> inf.stats.ContainsKey stat)
                |> Seq.map (fun inf -> inf.stats.TryGetValue stat)

            let statMod =
                if Seq.isEmpty elligibles
                then None
                else elligibles |> Seq.fold (+) StatMod.empty |> Some

            do infusionsStatModCache.Add(stat, statMod)

    member this.CalculateSlotCountFor(qc: QualityCategory) =
        let apparelProps = Option.ofObj this.parent.def.apparel

        // limit by body part group count
        let limit =
            if Settings.SlotModifiers.bodyPartHandle.Value then
                apparelProps
                |> Option.map (fun a -> a.bodyPartGroups.Count)
                |> Option.defaultValue Int32.MaxValue
            elif qc < QualityCategory.Normal then
                0
            else
                Int32.MaxValue

        let layerBonus =
            apparelProps
            |> Option.map (fun a -> if Settings.SlotModifiers.layerHandle.Value then a.layers.Count - 1 else 0)
            |> Option.defaultValue 0

        min limit (Settings.Slots.getBaseSlotsFor qc)
        + layerBonus

    member this.GetModForStat(stat: StatDef) =
        do this.PopulateInfusionsStatModCache(stat)
        infusionsStatModCache.TryGetValue(stat, None)
        |> Option.defaultValue StatMod.empty

    member this.HasInfusionForStat(stat: StatDef) =
        do this.PopulateInfusionsStatModCache(stat)
        infusionsStatModCache.TryGetValue(stat, None)
        |> Option.isSome

    member this.InvalidateCache() =
        do infusionsStatModCache.Clear()

           bestInfusionCache <- this.Infusions |> Seq.tryHead
           onHitsCache <-
               infusions
               |> Seq.map (fun inf -> inf.OnHits)
               |> Seq.choose id
               |> Seq.concat
               |> List.ofSeq
               |> Some


    member this.MarkForInfuser(infDef: InfusionDef) = do this.WantingSet <- Set.add infDef wantingSet
    member this.MarkForExtractor(infDef: InfusionDef) = do this.ExtractionSet <- Set.add infDef extractionSet
    member this.MarkForRemoval(infDef: InfusionDef) = do this.RemovalSet <- Set.add infDef removalSet

    member this.UnmarkForInfuser(infDef: InfusionDef) = do this.WantingSet <- Set.remove infDef wantingSet
    member this.UnmarkForExtractor(infDef: InfusionDef) = do this.ExtractionSet <- Set.remove infDef extractionSet
    member this.UnmarkForRemoval(infDef: InfusionDef) = do this.RemovalSet <- Set.remove infDef removalSet

    member this.FinalizeSetMutations() =
        if Set.isEmpty wantingSet
        then CompInfusion.UnregisterWantingCandidates this
        else CompInfusion.RegisterWantingCandidate this

        if Set.isEmpty extractionSet
        then CompInfusion.UnregisterExtractionCandidates this
        else CompInfusion.RegisterExtractionCandidate this

        if Set.isEmpty removalSet
        then CompInfusion.UnregisterRemovalCandidate this
        else CompInfusion.RegisterRemovalCandidate this

    member this.MakeBestInfusionLabel length =
        match this.BestInfusion with
        | Some bestInf ->
            let label =
                match length with
                | Long -> bestInf.label
                | Short -> bestInf.LabelShort

            if this.Size > 1 then
                StringBuilder(label).Append("(+").Append(this.Size - 1).Append(")")
                |> string
            else
                label
        | None -> ""

    // overrides below
    override this.TransformLabel label =
        match this.BestInfusion with
        | Some bestInf ->
            let parent = this.parent

            let isInfuser =
                Option.ofObj parent.def.tradeTags
                |> Option.map (Seq.contains "Infusion_Infuser")
                |> Option.defaultValue false

            let baseLabel =
                GenLabel.ThingLabel(parent.def, parent.Stuff)

            let sb =
                match bestInf.position with
                | Position.Prefix -> translate2 "Infusion.Label.Prefixed" (this.MakeBestInfusionLabel Long) baseLabel
                | Position.Suffix -> translate2 "Infusion.Label.Suffixed" (this.MakeBestInfusionLabel Long) baseLabel
                | _ -> raise (ArgumentException("Position must be either Prefix or Suffix"))
                |> string
                |> StringBuilder

            // components
            // quality should never be None but let's be cautious
            let quality =
                Comp.ofThing<CompQuality> parent
                |> Option.map (fun cq -> cq.Quality.GetLabel())

            let hitPoints =
                if parent.def.useHitPoints
                   && parent.HitPoints < parent.MaxHitPoints
                   && parent.def.stackLimit = 1 then
                    Some
                        ((float32 parent.HitPoints
                          / float32 parent.MaxHitPoints).ToStringPercent())
                else
                    None

            let tainted =
                match parent with
                | :? Apparel as apparel -> if apparel.WornByCorpse then Some(translate "WornByCorpseChar") else None
                | _ -> None

            // infuser applicability
            let applicability =
                if isInfuser then
                    this.BestInfusion
                    |> Option.map InfusionDef.makeRequirementString
                else
                    None

            do [ applicability
                 quality
                 hitPoints
                 tainted ]
               |> List.choose id
               |> String.concat " "
               |> (fun str ->
                   if not (str.NullOrEmpty())
                   then sb.Append(" (").Append(str).Append(")") |> ignore)

            string sb
        | None -> label

    override this.PostSpawnSetup(respawningAfterLoad) =
        if not respawningAfterLoad
           && slotCount = -1
           && quality >= QualityCategory.Normal then
            do slotCount <- this.CalculateSlotCountFor quality

        if not (respawningAfterLoad || Seq.isEmpty removalSet)
        then do CompInfusion.RegisterRemovalCandidate this

    override this.PostDeSpawn(_) = do CompInfusion.UnregisterRemovalCandidate this

    override this.GetDescriptionPart() = this.Descriptions

    override this.DrawGUIOverlay() =
        if Find.CameraDriver.CurrentZoom
           <= CameraZoomRange.Close then
            match this.BestInfusion with
            | Some bestInf ->
                do GenMapUI.DrawThingLabel
                    (GenMapUI.LabelDrawPosFor(this.parent, -0.6499999762f),
                     this.MakeBestInfusionLabel Short,
                     bestInf.tier.color)
            | None -> ()

    override this.PostExposeData() =
        Scribe.value "quality" this.Quality
        |> Option.iter (fun qc -> do this.Quality <- qc)

        Scribe.valueDefault "effectsEnabled" true effectsEnabled
        |> Option.iter (fun e -> do effectsEnabled <- e)

        Scribe.valueDefault "slotCount" (this.CalculateSlotCountFor quality) this.SlotCount
        |> Option.iter (fun sc -> do slotCount <- sc)

        Scribe.defCollection "infusion" infusions
        |> Option.iter (fun infs ->
            do this.Infusions <-
                infs
                |> Seq.filter (InfusionDef.gracefullyDies >> not)
                |> Seq.map (fun inf ->
                    inf.Migration
                    |> Option.bind (fun m -> m.Replace)
                    |> Option.defaultValue inf))

        Scribe.defCollection "wanting" wantingSet
        |> Option.map Set.ofSeq
        |> Option.iter (fun infs -> do wantingSet <- infs)

        Scribe.defCollection "removal" removalSet
        |> Option.map Set.ofSeq
        |> Option.iter (fun infs ->
            do removalSet <-
                infs
                |> Set.filter (InfusionDef.gracefullyDies >> not))

    override this.AllowStackWith(other) =
        Comp.ofThing<CompInfusion> other
        |> Option.map (fun comp -> infusions = comp.InfusionsRaw)
        |> Option.defaultValue false

    override this.PostSplitOff(other) =
        do Comp.ofThing<CompInfusion> other
           |> Option.iter (fun comp -> do comp.Infusions <- this.Infusions)

    override this.GetHashCode() = this.parent.thingIDNumber

    override this.Equals(ob) =
        match ob with
        | :? CompInfusion as comp -> this.parent.thingIDNumber = comp.parent.thingIDNumber
        | _ -> false

    interface IComparable with
        member this.CompareTo(ob) =
            match ob with
            | :? CompInfusion as comp ->
                let thingID = comp.parent.ThingID
                this.parent.ThingID.CompareTo thingID
            | _ -> 0


module CompInfusion =
    let addInfusion infDef (comp: CompInfusion) =
        do comp.Infusions <-
            seq {
                yield infDef
                yield! comp.Infusions
            }

    let setInfusions infDefs (comp: CompInfusion) = do comp.Infusions <- infDefs

    /// Picks elligible `InfusionDef` for the `Thing`.
    let pickInfusions quality (comp: CompInfusion) =
        // chance
        let checkChance (infDef: InfusionDef) =
            let chance =
                infDef.ChanceFor(quality)
                * Settings.SelectionConsts.chanceHandle.Value

            Rand.Chance chance

        DefDatabase<InfusionDef>.AllDefs
        |> Seq.filter (fun infDef -> Settings.Tiers.isEnabled infDef.tier)
        |> Seq.filter
            (InfusionDef.activeForUse
             <&> InfusionDef.checkAllComplexes comp.parent quality)
        // -> (infusionDef * weight)
        |> Seq.map (fun infDef ->
            (infDef,
             (infDef.WeightFor quality)
             * Settings.SelectionConsts.weightHandle.Value
             + Rand.Value)) // weighted, duh
        |> Seq.sortByDescending snd
        |> Seq.truncate comp.SlotCount
        |> Seq.map fst
        |> Seq.filter checkChance
        |> List.ofSeq // need to "finalize" the random sort
        |> List.sort

    let rerollInfusions (comp: CompInfusion) =
        (pickInfusions comp.Quality comp |> setInfusions) comp

    let removeMarkedInfusions (comp: CompInfusion) =
        do comp.Infusions <- Set.difference comp.InfusionsRaw comp.RemovalSet
        do comp.RemovalSet <- Set.empty // maybe not needed

    let removeAllInfusions (comp: CompInfusion) = do comp.Infusions <- Set.empty

    let removeInfusion def (comp: CompInfusion) =
        do comp.Infusions <- Set.remove def comp.InfusionsRaw
