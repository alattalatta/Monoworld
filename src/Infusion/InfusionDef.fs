namespace Infusion

open System
open System.Collections.Generic
open System.Text

open Poet.Lib
open RimWorld
open Verse
open UnityEngine

open DefFields
open StatMod
open Infusion.Matchers
open Infusion.OnHitWorkers


[<AllowNullLiteral>]
type InfusionDef =
    inherit HashEqualDef

    /// Label for map overlay.
    val mutable labelShort: string

    /// Descriptions for special effects.
    val mutable extraDescriptions: ResizeArray<string>

    /// Matcher filters.
    val mutable matches: ResizeArray<Matcher<InfusionDef>>

    /// On-hit effect workers.
    val mutable onHits: ResizeArray<OnHitWorker>

    /// Will not used for new infusions.
    val mutable disabled: bool

    /// Will migrate itself, by removing or replacing itself.
    val mutable migration: Migration<InfusionDef>

    /// Postfix or Suffix.
    val mutable position: Position

    /// The tier of this infusion.
    val mutable tier: TierDef

    val mutable stats: Dictionary<StatDef, StatMod>

    new() =
        { inherit HashEqualDef()
          labelShort = ""
          extraDescriptions = ResizeArray()

          matches = ResizeArray()
          onHits = null

          disabled = false
          migration = null
          position = Position.Prefix
          stats = Dictionary()
          tier = TierDef.empty }

    member this.LabelShort =
        if this.labelShort.NullOrEmpty() then
            this.label
        else
            this.labelShort

    member this.OnHits = Option.ofObj this.onHits

    member this.Migration = Option.ofObj this.migration

    member this.ChanceFor(quality: QualityCategory) = valueFor quality this.tier.chances

    member this.WeightFor(quality: QualityCategory) = valueFor quality this.tier.weights

    override this.ToString() =
        sprintf "%s (%s, %s)" (base.ToString()) this.label this.tier.label

    override this.Equals(ob) = base.Equals(ob)

    override this.GetHashCode() = base.GetHashCode()

    interface IComparable with
        member this.CompareTo(ob) =
            match ob with
            | :? InfusionDef as infDef ->
                let byTierPriority =
                    this.tier.priority.CompareTo infDef.tier.priority

                if byTierPriority <> 0 then
                    byTierPriority
                else
                    this.defName.CompareTo infDef.defName
            | _ -> 0


module InfusionDef =
    let activeForUse (infDef: InfusionDef) =
        not infDef.disabled && isNull infDef.migration

    let gracefullyDies (infDef: InfusionDef) =
        infDef.Migration
        |> Option.map (fun m -> m.remove)
        |> Option.defaultValue false

    let matchesAll target quality (infDef: InfusionDef) =
        (infDef.ChanceFor quality) > 0.0f
        && infDef.matches.TrueForAll(fun matcher -> matcher.Match target infDef)

    let makeRequirementString (infDef: InfusionDef) =
        infDef.matches
        |> Seq.map (fun matcher -> matcher.RequirementString)
        |> Seq.choose id
        |> String.concat ", "

    let makeDescriptionString (infDef: InfusionDef) =
        let labelSB = string infDef.LabelCap |> StringBuilder
        let reqString = makeRequirementString infDef

        do
            labelSB.Append(" (").Append(infDef.tier.label)
            |> ignore

        if String.length reqString > 0 then
            do labelSB.Append(" ― ").Append(reqString) |> ignore

        let label = labelSB.Append(")") |> string

        let statsDescriptions =
            dictseq infDef.stats
            |> Seq.fold
                (fun (acc: StringBuilder) cur ->
                    acc
                        .Append("\n  ")
                        .Append(cur.Key.LabelCap)
                        .Append(" ... ")
                        .Append((stringForStat cur.Key cur.Value)))
                (StringBuilder())

        let extraDescriptions =
            if (infDef.extraDescriptions.NullOrEmpty()) then
                ""
            else
                infDef.extraDescriptions
                |> Seq.fold (fun (acc: StringBuilder) cur -> acc.Append("\n  ").Append(cur)) (StringBuilder())
                |> string

        StringBuilder(label.Colorize(infDef.tier.color))
            .Append(statsDescriptions)
            .Append(extraDescriptions.Colorize(Color(0.11f, 1.0f, 0.0f)))
        |> string
