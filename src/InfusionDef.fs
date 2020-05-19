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
open Infusion.Complex

[<AllowNullLiteral>]
type InfusionDef =
    inherit HashEqualDef

    /// Label for map overlay.
    val mutable labelShort: string

    /// Descriptions for special effects.
    val mutable extraDescriptions: ResizeArray<string>

    val mutable complexes: ResizeArray<Complex<InfusionDef>>

    val mutable disabled: bool
    val mutable migration: Migration<InfusionDef>
    val mutable extraDamages: ResizeArray<ExtraDamage>
    val mutable extraExplosions: ResizeArray<ExtraExplosion>
    val mutable position: Position
    val mutable stats: Dictionary<StatDef, StatMod>
    val mutable tier: TierDef

    new() =
        { inherit HashEqualDef()
          labelShort = ""
          extraDescriptions = ResizeArray()

          complexes = ResizeArray()

          disabled = false
          migration = null
          extraDamages = null
          extraExplosions = null
          position = Position.Prefix
          stats = Dictionary()
          tier = TierDef.empty }

    member this.LabelShort = if this.labelShort.NullOrEmpty() then this.label else this.labelShort

    member this.ExtraDamages = Option.ofObj this.extraDamages

    member this.ExtraExplosions = Option.ofObj this.extraExplosions

    member this.Migration = Option.ofObj this.migration

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
                |> Seq.fold (fun (acc: StringBuilder) cur -> acc.Append("\n  ").Append(cur)) (StringBuilder())
                |> string

        string
            (StringBuilder(label.Colorize(this.tier.color)).Append(statsDescriptions)
                .Append(extraDescriptions.Colorize(Color(0.11f, 1.0f, 0.0f))))

    override this.ToString() = sprintf "%s (%s)" (base.ToString()) this.label

    override this.Equals(ob) = base.Equals(ob)
    override this.GetHashCode() = base.GetHashCode()

    interface IComparable with
        member this.CompareTo(ob) =
            match ob with
            | :? InfusionDef as infDef ->
                let byTierPriority =
                    this.tier.priority.CompareTo infDef.tier.priority

                if byTierPriority <> 0 then byTierPriority else this.defName.CompareTo infDef.defName
            | _ -> 0

module InfusionDef =

    let gracefullyDie (infDef: InfusionDef) =
        infDef.Migration
        |> Option.map (fun m -> m.remove)
        |> Option.defaultValue false

    let disabled (infDef: InfusionDef) = gracefullyDie infDef || infDef.disabled

    let collectExtraEffects (getter: InfusionDef -> 'a option) (infusions: Set<InfusionDef>) =
        infusions
        |> Seq.map getter
        |> Seq.choose id
        |> Seq.concat
        |> Some

    let checkAllComplexes target quality (infDef: InfusionDef) =
        (infDef.ChanceFor quality) > 0.0f
        && infDef.complexes.TrueForAll(fun complex -> complex.Match target infDef)
