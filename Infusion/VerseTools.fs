module Infusion.VerseTools

open Poet.Lib
open Poet.Lyric
open RimWorld
open Verse


// Because StatDef doesn't implement IComparable,
// defs can't be used directly for Sets.
let accuracyStats =
    Set.ofList [ StatDefOf.AccuracyTouch.defName
                 StatDefOf.AccuracyShort.defName
                 StatDefOf.AccuracyMedium.defName
                 StatDefOf.AccuracyLong.defName ]

// Same as StatDef.
let pawnStatCategories =
    Set.ofList [ StatCategoryDefOf.BasicsPawn.defName
                 StatCategoryDefOf.BasicsPawnImportant.defName
                 StatCategoryDefOf.PawnCombat.defName
                 StatCategoryDefOf.PawnMisc.defName
                 StatCategoryDefOf.PawnSocial.defName
                 StatCategoryDefOf.PawnWork.defName ]

let resetHP<'T when 'T :> Thing> (thing: 'T) =
    do thing.HitPoints <- thing.MaxHitPoints

let upcastToThing a = a :> Thing

module Pawn =
    let isAliveAndWell thing =
        if Thing.isDestroyed thing then
            false
        else
            tryCast<Pawn> thing
            |> Option.map (not << Pawn.isDead)
            |> Option.defaultValue false
