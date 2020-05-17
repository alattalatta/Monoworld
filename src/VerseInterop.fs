module Infusion.VerseInterop

open RimWorld
open Verse
open Verse.AI

let apparelsOfPawn (pawn: Pawn): option<list<Apparel>> =
    Option.ofObj pawn.apparel
    |> Option.map (fun tracker -> List.ofSeq tracker.WornApparel)

let equipmentsOfPawn (pawn: Pawn): option<list<ThingWithComps>> =
    Option.ofObj pawn.equipment
    |> Option.map (fun tracker -> List.ofSeq tracker.AllEquipmentListForReading)

let compOfThing<'a when 'a :> ThingComp and 'a: null> (thing: Thing) = Option.ofObj (thing.TryGetComp<'a>())

let parentOfComp (comp: ThingComp) = comp.parent

let translate (key: string) = key.TranslateSimple()

let translate1 (key: string) a = key.Translate(NamedArgument(a, null))

let translate2 (key: string) a b =
    key.Translate(NamedArgument(a, null), NamedArgument(b, null))

module DamageInfo =
    let setAngle angle (di: DamageInfo) =
        di.SetAngle angle
        di

    let setBodyRegion height depth (di: DamageInfo) =
        di.SetBodyRegion(height, depth)
        di

    let setWeaponBodyPartGroup bodyPartGroup (di: DamageInfo) =
        di.SetWeaponBodyPartGroup(bodyPartGroup)
        di

module Toil =
    let setInitAction fn (toil: Toil) =
        do toil.initAction <- new System.Action(fn)
        toil

    let setTickAction fn (toil: Toil) =
        do toil.tickAction <- new System.Action(fn)
        toil

    let addFailOn fn (toil: Toil) =
        do toil.FailOn(new System.Func<bool>(fn)) |> ignore
        toil

    let addFailOnCannotTouch targetIndex pathEndMode (toil: Toil) =
        do toil.FailOnCannotTouch(targetIndex, pathEndMode)
           |> ignore
        toil

    let addFailOnSomeoneInteracting targetIndex (toil: Toil) =
        do toil.FailOnSomeonePhysicallyInteracting(targetIndex)
           |> ignore
        toil

    let setDefaultCompleteMode mode (toil: Toil) =
        do toil.defaultCompleteMode <- mode
        toil

    let addProgressBar targetIndex (toil: Toil) =
        do toil.WithProgressBarToilDelay(targetIndex)
           |> ignore
        toil
