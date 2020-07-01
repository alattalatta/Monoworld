module Infusion.StatPart

open System.Text

open Poet.Lyric
open Poet.Lyric.Translation
open RimWorld
open Verse
open UnityEngine

open StatMod
open VerseTools


// Handles stat transformation.
type Infusion =
    inherit StatPart

    val IsPawnStat: bool

    new(stat: StatDef) =
        { inherit StatPart(parentStat = stat)
          IsPawnStat =
              Option.ofObj stat.category
              |> Option.map (fun catDef -> Set.contains catDef.defName pawnStatCategories)
              |> Option.defaultValue false }

    override this.TransformValue(req, value: byref<float32>) =
        if not this.IsPawnStat then
            do value <-
                match req.Thing with
                | null -> value
                | thing -> this.TransformThingStat(value, thing)

            if Set.contains this.parentStat.defName accuracyStats
               && (value >= 1.0f) then
                if Settings.AccuracyOvercap.handle.Value then
                    let overcaps =
                        if value <= 1.1f then
                            value - 1.0f
                        else
                            Mathf.Log((value - 1.0f) * 10.0f + 1.0f, 2.0f)
                            / 10.0f

                    do value <- 1.0f + overcaps
                else
                    do value <- 1.0f

    override this.ExplanationPart req =
        if not this.IsPawnStat then
            match req.Thing with
            | null -> null
            | thing -> this.ExplainForThing thing
        else
            null

    // just for things
    // Harmonize.StatWorker handles pawn stats
    member private this.TransformThingStat(value, thing: Thing) =
        this.GetAllModifiersFromThing thing
        |> applyTo value

    member private this.ExplainForThing(thing: Thing) =
        let sb = StringBuilder()

        let statModSum = this.GetAllModifiersFromThing thing

        if statModSum <> StatMod.empty then
            do sb.Append(translate "Infusion.StatPart.Start")
                 .Append(stringForStat this.parentStat (this.GetAllModifiersFromThing thing))
               |> ignore

        string sb

    member private this.GetAllModifiersFromThing<'T when 'T :> Thing>(thing: 'T): StatMod =
        Comp.ofThing<CompInfusion> thing
        |> Option.map (fun comp -> comp.GetModForStat this.parentStat)
        |> Option.defaultValue StatMod.empty
