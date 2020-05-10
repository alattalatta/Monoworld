namespace Infusion

open System
open System.Collections.Generic
open System.Text

open RimWorld
open Verse

open DefFields
open VerseTools
open Lib
open StatMod

// The definition.
type InfusionDef =
    inherit Def

    /// Label for map overlay.
    val mutable labelShort: string

    /// Descriptions for special effects.
    val mutable extraDescriptions: ResizeArray<string>

    val mutable chances: QualityMap
    val mutable disabled: bool
    val mutable extraDamages: ResizeArray<ExtraDamage>
    val mutable position: Position
    val mutable requirements: Requirements
    val mutable stats: Dictionary<StatDef, StatMod>
    val mutable tier: Tier // [todo] Replace with InfusionTierDef: color, label, priority
    val mutable weights: QualityMap

    new() =
        { inherit Def()
          labelShort = ""
          extraDescriptions = ResizeArray()

          chances = QualityMap()
          disabled = false
          extraDamages = null
          position = Position.Prefix
          requirements = Requirements()
          stats = Dictionary()
          tier = Tier.Common
          weights = QualityMap() }

    member this.LabelShort = if this.labelShort.NullOrEmpty() then this.label else this.labelShort

    member this.ExtraDamages = Option.ofObj this.extraDamages

    member this.ChanceFor(quality: QualityCategory) =
        match quality with
        | QualityCategory.Awful -> this.chances.awful
        | QualityCategory.Poor -> this.chances.poor
        | QualityCategory.Normal -> this.chances.normal
        | QualityCategory.Good -> this.chances.good
        | QualityCategory.Excellent -> this.chances.excellent
        | QualityCategory.Masterwork -> this.chances.masterwork
        | QualityCategory.Legendary -> this.chances.legendary
        | _ -> 0.0f

    member this.WeightFor(quality: QualityCategory) =
        match quality with
        | QualityCategory.Awful -> this.weights.awful
        | QualityCategory.Poor -> this.weights.poor
        | QualityCategory.Normal -> this.weights.normal
        | QualityCategory.Good -> this.weights.good
        | QualityCategory.Excellent -> this.weights.excellent
        | QualityCategory.Masterwork -> this.weights.masterwork
        | QualityCategory.Legendary -> this.weights.legendary
        | _ -> 0.0f

    member this.GetDescriptionString() =
        let label =
            ((StringBuilder(string (this.LabelCap)).Append(" (").Append(getLabelOfTier this.tier).Append(") :"))
             |> string)

        let statsDescriptions =
            dictseq this.stats
            |> Seq.fold (fun (acc: StringBuilder) cur ->
                acc.Append("\n  ").Append(cur.Key.LabelCap).Append(" ... ").Append((stringForStat cur.Key cur.Value)))
                   (StringBuilder())

        let extraDescriptions =
            if (this.extraDescriptions.NullOrEmpty()) then
                ""
            else
                this.extraDescriptions
                |> Seq.fold (fun (acc: StringBuilder) cur -> acc.Append("\n  ").AppendLine(cur)) (StringBuilder())
                |> string

        string
            (StringBuilder(label.Colorize(tierToColor this.tier)).Append(statsDescriptions)
                .Append(extraDescriptions.Colorize(tierToColor Tier.Uncommon)))

    override this.Equals(ob: obj) =
        match ob with
        | :? InfusionDef as infDef -> this.defName = infDef.defName
        | _ -> false

    override this.GetHashCode() = this.defName.GetHashCode()

    override this.ToString() = sprintf "%s (%s)" (base.ToString()) this.label

    interface IComparable with
        member this.CompareTo(ob: obj) =
            match ob with
            | :? InfusionDef as infDef ->
                let byTier = this.tier.CompareTo infDef.tier
                if byTier <> 0 then byTier else this.defName.CompareTo infDef.defName
            | _ -> 0
