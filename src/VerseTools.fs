module Infusion.VerseTools

open RimWorld
open Verse

// Because StatDef doesn't implement IComparable,
// defs can't be used directly for Sets.
let accuracyStats =
    Set.ofList
        [ StatDefOf.AccuracyTouch.defName
          StatDefOf.AccuracyShort.defName
          StatDefOf.AccuracyMedium.defName
          StatDefOf.AccuracyLong.defName ]

// Same as StatDef.
let pawnStatCategories =
    Set.ofList
        [ StatCategoryDefOf.BasicsPawn.defName
          StatCategoryDefOf.BasicsPawnImportant.defName
          StatCategoryDefOf.PawnCombat.defName
          StatCategoryDefOf.PawnMisc.defName
          StatCategoryDefOf.PawnSocial.defName
          StatCategoryDefOf.PawnWork.defName ]

/// Predicate for determining whether given ThingDef is an apparel, a weapon, or not.
let apparelOrWeapon (def: ThingDef) =
    ThingCategoryDefOf.Apparel.ContainedInThisOrDescendant def
    || ThingCategoryDefOf.Weapons.ContainedInThisOrDescendant def

let resetHP<'T when 'T :> Thing> (thing: 'T) = do thing.HitPoints <- thing.MaxHitPoints

let upcastToThing a = a :> Thing

/// Scribes a value.
///
/// When saving, returns None.
///
/// When loading, if the saved data exists, returns it in Some. Otherwise returns None.
let scribeValue key value =
    let mutable out = value

    match Scribe.mode with
    | LoadSaveMode.LoadingVars ->
        do Scribe_Values.Look(&out, key)
        Some out
    | LoadSaveMode.Saving ->
        do Scribe_Values.Look(&out, key)
        None
    | _ -> None

/// Scribes a value, with the default value.
///
/// When saving, returns None.
///
/// When loading, if the saved data exists, returns it in Some. Otherwise returns None.
let scribeValueWithDefault key defaultValue value =
    let mutable out = value

    match Scribe.mode with
    | LoadSaveMode.LoadingVars ->
        do Scribe_Values.Look(&out, key, defaultValue)
        Some out
    | LoadSaveMode.Saving ->
        do Scribe_Values.Look(&out, key, defaultValue)
        None
    | _ -> None

/// Scribes a nullable value.
///
/// When saving, returns None.
///
/// When loading, if the saved data exists, returns it in Some. Otherwise returns None.
let scribeValueNullable key value =
    let mutable out = value

    match Scribe.mode with
    | LoadSaveMode.LoadingVars ->
        do Scribe_Values.Look(&out, key)
        Option.ofObj out
    | LoadSaveMode.Saving ->
        do Scribe_Values.Look(&out, key)
        None
    | _ -> None

/// Scribes a sequence of defs.
///
/// When saving, returns None.
///
/// When loading, if the saved data exists, returns it in Some. Otherwise returns None.
let scribeDefCollection<'a when 'a :> Def> key (defs: 'a seq) =
    let mutable out = ResizeArray defs

    match Scribe.mode with
    | LoadSaveMode.LoadingVars ->
        do Scribe_Collections.Look(&out, key, LookMode.Def)
        Option.ofObj out
        |> Option.map (fun a -> seq { yield! a })
    | LoadSaveMode.Saving ->
        do Scribe_Collections.Look(&out, key, LookMode.Def)
        None
    | _ -> None
