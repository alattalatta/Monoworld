module Infusion.VerseTools

open Poet.Lib
open Poet.Lyric
open RimWorld
open Verse


// Because StatDef doesn't implement IComparable,
// defs can't be used directly for Sets.
// Thus we use their defNames instead.
/// A set of defNames of all accuracy stats.
let accuracyStats =
  Set.ofList [ StatDefOf.AccuracyTouch.defName
               StatDefOf.AccuracyShort.defName
               StatDefOf.AccuracyMedium.defName
               StatDefOf.AccuracyLong.defName ]


// Same as above.
/// A set of defNames of all direct armor stats.
let armorStats =
  Set.ofList [ StatDefOf.ArmorRating_Blunt.defName
               StatDefOf.ArmorRating_Heat.defName
               StatDefOf.ArmorRating_Sharp.defName ]


// Same as above.
/// A set of defNames of all Pawn stat categories.
let pawnStatCategories =
  Set.ofList [ StatCategoryDefOf.BasicsPawn.defName
               StatCategoryDefOf.BasicsPawnImportant.defName
               StatCategoryDefOf.PawnCombat.defName
               StatCategoryDefOf.PawnMisc.defName
               StatCategoryDefOf.PawnSocial.defName
               StatCategoryDefOf.PawnWork.defName ]


/// Resets a Thing's HitPoints to its MaxHitPoints.
let resetHP<'T when 'T :> Thing> (thing: 'T) =
  do thing.HitPoints <- thing.MaxHitPoints

let upcastToThing a = a :> Thing

module Pawn =
  /// Checks if a Pawn is alive and not destroyed.
  let isAliveAndWell thing =
    if Thing.isDestroyed thing then
      false
    else
      tryCast<Pawn> thing
      |> Option.map (not << Pawn.isDead)
      |> Option.defaultValue false


module Verb =
  let getAdjustedMeleeDamage (verb: Verb) =
    verb.verbProps.AdjustedMeleeDamageAmount(verb, verb.CasterPawn)