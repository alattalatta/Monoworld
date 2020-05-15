module Infusion.VerseTools

open RimWorld
open UnityEngine
open Verse

open DefFields
open VerseInterop

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

let apparelOrWeapon (def: ThingDef) =
    ThingCategoryDefOf.Apparel.ContainedInThisOrDescendant def
    || ThingCategoryDefOf.Weapons.ContainedInThisOrDescendant def

let rec isToolCapableOfDamageType (dt: DamageType) (tool: Tool) =
    match dt with
    | DamageType.Anything -> true
    | DamageType.Blunt ->
        tool.capacities
        |> Seq.exists (fun capacity ->
            capacity.defName = "Blunt"
            || capacity.defName = "Poke")
    | DamageType.Sharp -> not (isToolCapableOfDamageType DamageType.Blunt tool) // assuming reverse of blunt is sharp...
    | _ -> false

let resetHP<'T when 'T :> Thing and 'T: null> (thing: 'T) = do thing.HitPoints <- thing.MaxHitPoints

let scribeDefCollection key (defs: seq<'a>): option<seq<'a>> =
    let mutable out = ResizeArray defs

    if (Scribe.mode = LoadSaveMode.LoadingVars
        || (Scribe.mode = LoadSaveMode.Saving
            && not (out.NullOrEmpty()))) then
        do Scribe_Collections.Look(&out, key, LookMode.Def)

    Option.ofObj out
    |> Option.map (fun a -> seq { yield! a })
