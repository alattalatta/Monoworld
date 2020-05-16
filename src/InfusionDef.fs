namespace Infusion

open System
open System.Collections.Generic
open System.Text

open RimWorld
open Verse
open UnityEngine

open DefFields
open Lib
open StatMod

type InfusionDef =
    inherit Def

    /// Label for map overlay.
    val mutable labelShort: string

    /// Descriptions for special effects.
    val mutable extraDescriptions: ResizeArray<string>

    val mutable disabled: bool
    /// Removes itself when loading. Implies disabled.
    val mutable gracefullyDie: bool
    val mutable extraDamages: ResizeArray<ExtraDamage>
    val mutable extraExplosions: ResizeArray<ExtraExplosion>
    val mutable position: Position
    val mutable requirements: Requirements
    val mutable stats: Dictionary<StatDef, StatMod>
    val mutable tier: TierDef

    new() =
        { inherit Def()
          labelShort = ""
          extraDescriptions = ResizeArray()

          disabled = false
          gracefullyDie = false
          extraDamages = null
          extraExplosions = null
          position = Position.Prefix
          requirements = Requirements()
          stats = Dictionary()
          tier = null }

    member this.LabelShort = if this.labelShort.NullOrEmpty() then this.label else this.labelShort

    member this.ExtraDamages = Option.ofObj this.extraDamages

    member this.ExtraExplosions = Option.ofObj this.extraExplosions

    member this.ChanceFor(quality: QualityCategory) = valueFor quality this.tier.chances

    member this.WeightFor(quality: QualityCategory) = valueFor quality this.tier.weights

    member this.GetDescriptionString() =
        let label =
            ((StringBuilder(string (this.LabelCap)).Append(" (").Append(this.tier.label).Append(") :"))
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
            (StringBuilder(label.Colorize(this.tier.color)).Append(statsDescriptions)
                .Append(extraDescriptions.Colorize(Color(0.11f, 1.0f, 0.0f))))

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
                let byTierPriority =
                    this.tier.priority.CompareTo infDef.tier.priority

                if byTierPriority <> 0 then byTierPriority else this.defName.CompareTo infDef.defName
            | _ -> 0

module InfusionDef =
    let gracefullyDie (infDef: InfusionDef) = infDef.gracefullyDie

    let disabled (infDef: InfusionDef) = gracefullyDie infDef || infDef.disabled
