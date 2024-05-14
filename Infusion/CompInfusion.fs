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

  let mutable biocoder: CompBiocodable option = None
  let mutable effectsEnabled = true
  let mutable slotCount = -1
  let mutable quality = QualityCategory.Normal

  let mutable bestInfusionCache = None
  let mutable onHitsCache = None
  let infusionsStatModCache = Dictionary<StatDef, StatMod option>()

  static member WantingCandidates
    with get () = wantingCandidates
    and set value = do wantingCandidates <- value

  static member ExtractionCandidates
    with get () = extractionCandidates
    and set value = do extractionCandidates <- value

  static member RemovalCandidates
    with get () = removalCandidates
    and set value = do removalCandidates <- value

  static member ClearCaches() =
    wantingCandidates <- Set.empty
    extractionCandidates <- Set.empty
    removalCandidates <- Set.empty

  static member RegisterWantingCandidate comp =
    CompInfusion.WantingCandidates <- Set.add comp CompInfusion.WantingCandidates

  static member RegisterExtractionCandidate comp =
    CompInfusion.ExtractionCandidates <- Set.add comp CompInfusion.ExtractionCandidates

  static member RegisterRemovalCandidate comp =
    CompInfusion.RemovalCandidates <- Set.add comp CompInfusion.RemovalCandidates

  static member UnregisterWantingCandidates comp =
    CompInfusion.WantingCandidates <- Set.remove comp CompInfusion.WantingCandidates

  static member UnregisterExtractionCandidates comp =
    CompInfusion.ExtractionCandidates <- Set.remove comp CompInfusion.ExtractionCandidates

  static member UnregisterRemovalCandidate comp =
    CompInfusion.RemovalCandidates <- Set.remove comp CompInfusion.RemovalCandidates

  member this.BestInfusion = bestInfusionCache

  member this.Biocoder
    with get () = biocoder |> Option.filter (fun b -> b.Biocodable)
    and set (value: CompBiocodable option) = biocoder <- value

  member this.EffectsEnabled = effectsEnabled

  member this.Quality
    with get () = quality
    and set value =
      quality <- value
      slotCount <- this.CalculateSlotCountFor value

  member this.Infusions = infusions |> Seq.sortDescending

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

  member this.InfusionsByPosition =
    let (prefixes, suffixes) =
      this.Infusions
      |> Seq.fold
           (fun (pre, suf) cur ->
             if cur.position = Position.Prefix then
               (cur :: pre, suf)
             else
               (pre, cur :: suf))
           (List.empty, List.empty)

    (List.rev prefixes, List.rev suffixes)

  member this.Descriptions =
    this.Infusions
    |> Seq.map InfusionDef.makeDescriptionString
    |> String.concat "\n\n"

  // [todo] move under ITab
  member this.InspectionLabel =
    if Set.isEmpty infusions then
      (translate1 "Infusion.Label.NotInfused" this.parent.def.label)
        .CapitalizeFirst()
      |> string
    else
      let (prefixes, suffixes) = this.InfusionsByPosition

      let suffixedPart =
        if List.isEmpty suffixes then
          this.parent.def.label
        else
          let suffixString =
            (suffixes |> List.map (fun def -> def.label))
              .ToCommaList(true)

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

  // can't use CompGetGizmosExtra, Pawn_EquipmentTracker do not use them for Pawn Gizmos.
  member this.EffectGizmo =
    onHitsCache
    // List.length is not zero
    |> Option.filter (List.length >> (=) 0 >> not)
    |> Option.map (fun _ ->
      Command_Toggle(
        defaultLabel = ResourceBank.Strings.Gizmo.label,
        defaultDesc = ResourceBank.Strings.Gizmo.desc,
        icon = ResourceBank.Textures.Flame,
        isActive = (fun () -> effectsEnabled),
        toggleAction = (fun () -> effectsEnabled <- not effectsEnabled)
      ))

  member this.OnHits = Option.defaultValue List.empty onHitsCache

  member this.RemovalSet
    with get () = removalSet
    and set value =
      removalSet <- value
      extractionSet <- Set.difference extractionSet removalSet

      this.FinalizeSetMutations()

  member this.Size = Set.count infusions

  member this.SlotCount
    with get () = slotCount
    and set value = slotCount <- value

  member this.PopulateInfusionsStatModCache(stat: StatDef) =
    if not (infusionsStatModCache.ContainsKey stat) then
      let eligibles =
        infusions
        |> Seq.choose (fun inf -> Dict.get stat inf.stats)

      let statMod =
        if Seq.isEmpty eligibles then
          None
        else
          eligibles |> Seq.fold (+) StatMod.empty |> Some

      infusionsStatModCache.Add(stat, statMod)

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
      |> Option.map (fun a ->
        if Settings.SlotModifiers.layerHandle.Value then
          a.layers.Count - 1
        else
          0)
      |> Option.defaultValue 0

    min limit (Settings.Slots.getBaseSlotsFor qc)
    + layerBonus

  member this.GetModForStat(stat: StatDef) =
    this.PopulateInfusionsStatModCache(stat)

    infusionsStatModCache.TryGetValue(stat, None)
    |> Option.defaultValue StatMod.empty

  member this.HasInfusionForStat(stat: StatDef) =
    this.PopulateInfusionsStatModCache(stat)

    infusionsStatModCache.TryGetValue(stat, None)
    |> Option.isSome

  member this.InvalidateCache() =
    infusionsStatModCache.Clear()

    bestInfusionCache <- this.Infusions |> Seq.tryHead

    onHitsCache <-
      infusions
      |> List.ofSeq
      |> List.map (fun inf -> inf.OnHits)
      |> List.concat
      |> Some

  member this.MarkForInfuser(infDef: InfusionDef) =
    this.WantingSet <- Set.add infDef wantingSet

  member this.MarkForExtractor(infDef: InfusionDef) =
    this.ExtractionSet <- Set.add infDef extractionSet

  member this.MarkForRemoval(infDef: InfusionDef) =
    this.RemovalSet <- Set.add infDef removalSet

  member this.UnmarkForInfuser(infDef: InfusionDef) =
    this.WantingSet <- Set.remove infDef wantingSet

  member this.UnmarkForExtractor(infDef: InfusionDef) =
    this.ExtractionSet <- Set.remove infDef extractionSet

  member this.UnmarkForRemoval(infDef: InfusionDef) =
    this.RemovalSet <- Set.remove infDef removalSet

  member this.FinalizeSetMutations() =
    if Set.isEmpty wantingSet then
      CompInfusion.UnregisterWantingCandidates this
    else
      CompInfusion.RegisterWantingCandidate this

    if Set.isEmpty extractionSet then
      CompInfusion.UnregisterExtractionCandidates this
    else
      CompInfusion.RegisterExtractionCandidate this

    if Set.isEmpty removalSet then
      CompInfusion.UnregisterRemovalCandidate this
    else
      CompInfusion.RegisterRemovalCandidate this

  member this.MakeBestInfusionLabel length =
    match this.BestInfusion with
    | Some bestInf ->
      let label =
        match length with
        | Long -> bestInf.label
        | Short -> bestInf.LabelShort

      if this.Size > 1 then
        StringBuilder(label, label.Length + 5)
          .Append("(+")
          .Append(this.Size - 1)
          .Append(")")
        |> string
      else
        label
    | None -> ""

  member this.SetInfusions(value: InfusionDef seq, respawningAfterLoad: bool) =
    let originalHitPoints = float32 this.parent.HitPoints

    let hitPointsRatio =
      originalHitPoints
      / float32 this.parent.MaxHitPoints

    infusions <- value |> Set.ofSeq
    wantingSet <- Set.difference wantingSet infusions
    extractionSet <- Set.intersect extractionSet infusions
    removalSet <- Set.intersect removalSet infusions

    this.InvalidateCache()
    this.FinalizeSetMutations()

    // only increase HP along with max HP when already in game
    // the save contains both HP and max HP already
    if not respawningAfterLoad then
      this.parent.HitPoints <-
        (float32 this.parent.MaxHitPoints * hitPointsRatio)
        |> ceil
        |> int
        |> max (originalHitPoints |> round |> int)
        |> min this.parent.MaxHitPoints

  override this.DrawGUIOverlay() =
    if Find.CameraDriver.CurrentZoom
       <= CameraZoomRange.Close then
      match this.BestInfusion with
      | Some bestInf ->
        let pos =
          if Find.CameraDriver.CurrentZoom > CameraZoomRange.Closest
             || this.parent.def.defName.StartsWith "Infusion_" then
            -0.4f
          else
            -0.65f

        GenMapUI.DrawThingLabel(
          GenMapUI.LabelDrawPosFor(this.parent, pos),
          this.MakeBestInfusionLabel Short,
          bestInf.tier.color
        )
      | None -> ()

  // overrides parent label completely - expect conflicts
  // [todo] transpiler for GenLabel.ThingLabel
  override this.TransformLabel label =
    match this.BestInfusion with
    | Some bestInf ->
      let parent = this.parent

      let baseLabel =
        Option.ofObj parent.StyleDef
        |> Option.bind (fun def -> Option.ofObj def.overrideLabel)
        |> Option.defaultWith (fun () -> GenLabel.ThingLabel(parent.def, parent.Stuff))

      let translationKey =
        if bestInf.position = Position.Prefix then
          "Infusion.Label.Prefixed"
        else
          "Infusion.Label.Suffixed"

      let infusionLabel = this.MakeBestInfusionLabel Long

      let sb =
        translate2 translationKey infusionLabel baseLabel
        |> string
        |> StringBuilder

      // infuser applicability
      let isInfuser =
        Option.ofObj parent.def.tradeTags
        |> Option.map (Seq.contains "Infusion_Infuser")
        |> Option.defaultValue false

      if isInfuser then
        this.BestInfusion
        |> Option.map InfusionDef.makeRequirementString
        |> Option.iter (fun str -> sb.Append(" ").Append(str) |> ignore)

      sb.Append(GenLabel.LabelExtras(parent, true, true))
      |> string
    | None -> label

  override this.PostSpawnSetup(respawningAfterLoad) =
    if not respawningAfterLoad
       && slotCount = -1
       && quality >= QualityCategory.Normal then
      do slotCount <- this.CalculateSlotCountFor quality

    if not (respawningAfterLoad || Seq.isEmpty removalSet) then
      do CompInfusion.RegisterRemovalCandidate this

  override this.PostDeSpawn(_) =
    do CompInfusion.UnregisterRemovalCandidate this

  override this.GetDescriptionPart() = this.Descriptions

  override this.PostExposeData() =
    Scribe.value "quality" this.Quality
    |> Option.iter (fun qc -> do this.Quality <- qc)

    Scribe.valueDefault "effectsEnabled" true effectsEnabled
    |> Option.iter (fun e -> do effectsEnabled <- e)

    Scribe.valueDefault "slotCount" (this.CalculateSlotCountFor quality) this.SlotCount
    |> Option.iter (fun sc -> do slotCount <- sc)

    Scribe.defCollection "infusions" infusions
    |> Option.iter (fun infs ->
      let loadedInfusions =
        infs
        |> Seq.filter (InfusionDef.shouldRemoveItself >> not)
        |> Seq.map (fun inf ->
          inf.Migration
          |> Option.bind (fun m -> m.Replace)
          |> Option.defaultValue inf)

      this.SetInfusions(loadedInfusions, true))

    Scribe.defCollection "wanting" wantingSet
    |> Option.map Set.ofSeq
    |> Option.iter (fun infs -> do wantingSet <- infs)

    Scribe.defCollection "removal" removalSet
    |> Option.map Set.ofSeq
    |> Option.iter (fun infs ->
      do
        removalSet <-
          infs
          |> Set.filter (InfusionDef.shouldRemoveItself >> not))

  override this.AllowStackWith(_) = false

  override this.PostSplitOff(other) =
    do
      Comp.ofThing<CompInfusion> other
      |> Option.iter (fun comp -> do comp.SetInfusions(this.Infusions, false))

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
    do
      comp.SetInfusions(
        seq {
          yield infDef
          yield! comp.Infusions
        },
        false
      )

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
    |> Seq.filter (
      InfusionDef.activeForUse
      <&> InfusionDef.matchesAll comp.parent quality
    )
    |> List.ofSeq
    // -> (infusionDef * weight)
    |> List.map (fun infDef ->
      (infDef,
       Rand.Gaussian(
         Settings.SelectionConsts.muHandle.Value
         * (infDef.WeightFor quality),
         Settings.SelectionConsts.sigmaHandle.Value
       ))) // weighted, duh
    |> List.sortByDescending snd
    |> List.truncate comp.SlotCount
    |> List.filter (fst >> checkChance)
    |> List.map fst

  let setInfusions infDefs (comp: CompInfusion) = do comp.SetInfusions(infDefs, false)

  let rerollInfusions (comp: CompInfusion) =
    (pickInfusions comp.Quality comp |> setInfusions) comp

  let removeMarkedInfusions (comp: CompInfusion) =
    do comp.SetInfusions(Set.difference comp.InfusionsRaw comp.RemovalSet, false)
    do comp.RemovalSet <- Set.empty // maybe not needed

  let removeInfusion def (comp: CompInfusion) =
    do comp.SetInfusions(Set.remove def comp.InfusionsRaw, false)

  let forOnHitWorkers (thing: ThingWithComps) =
    Comp.ofThing<CompInfusion> thing
    |> Option.filter (fun comp -> comp.EffectsEnabled)
    |> Option.map (fun comp -> (comp.OnHits |> List.filter OnHitWorker.checkChance, comp))
