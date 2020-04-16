module Infusion.StatPart

open System.Text

open RimWorld
open Verse

open DefTool
open StatMod
open VerseInterop

// Handles stat transformation.
type Infusion =
    inherit StatPart

    val isPawnStat: bool

    new(stat: StatDef) =
        { inherit StatPart(parentStat = stat); isPawnStat = Set.contains stat.category.defName pawnStatCategories }

    override this.TransformValue(req, value: byref<float32>) =
        if not this.isPawnStat then
            let trv =
                match req.Thing with
                | null -> value
                | thing -> this.TransformThingStat(value, thing)

            do value <- trv

    override this.ExplanationPart req =
        if not this.isPawnStat then
            match req.Thing with
            | null -> null
            | thing -> this.ExplainForThing thing
        else
            null

    // just for things
    // Harmonize.StatWorker handles pawn stats
    member private this.TransformThingStat(value, thing: Thing) =
        this.GetAllModifiersFromThing thing
        |> List.fold (+) StatMod.empty
        |> applyTo value
        |> GenMath.RoundedHundredth

    member private this.ExplainForThing(thing: Thing) =
        let sb = StringBuilder()

        let statModSum = this.GetAllModifiersFromThing thing |> List.fold (+) StatMod.empty

        if statModSum <> StatMod.empty then
            do sb.Append(translate "Infusion.StatPart.Start").Append(statModSum) |> ignore

        string sb

    member private this.GetAllModifiersFromThings<'T when 'T :> Thing>(things: list<'T>): list<StatMod> =
        List.collect this.GetAllModifiersFromThing things

    member private this.GetAllModifiersFromThing<'T when 'T :> Thing>(thing: 'T): list<StatMod> =
        compOfThing<Comp.Infusion> thing
        |> Option.map (Comp.allModsForStat this.parentStat)
        |> Option.defaultValue List.empty
