module Infusion.Harmonize.VerbMeleeAttackDamage

open System.Collections.Generic

open HarmonyLib
open RimWorld
open Verse

open Infusion
open VerseInterop

[<HarmonyPatch(typeof<Verb_MeleeAttackDamage>, "DamageInfosToApply")>]
module DamageInfosToApply =
    let private createDamageInfo
        (instance: Verb_MeleeAttackDamage)
        (source: ThingWithComps)
        (targetPos: IntVec3)
        (extraDamage: ExtraDamage)
        =
        let amount = extraDamage.amount
        let pos = (targetPos - instance.caster.Position).ToVector3()

        DamageInfo
            (extraDamage.def, Rand.Range(amount * 0.8f, amount * 1.2f),
             extraDamage.AdjustedArmorPenetration(instance, instance.CasterPawn), -1.0f, instance.caster, null,
             source.def)
        |> DamageInfo.setAngle pos
        |> DamageInfo.setBodyRegion BodyPartHeight.Undefined BodyPartDepth.Outside
        |> DamageInfo.setWeaponBodyPartGroup (instance.verbProps.AdjustedLinkedBodyPartsGroup instance.tool)

    // by some unknown reasons, Postfix is always called twice
    // note that the flow isn't prefix1 -> postfix1 -> prefix2 -> postfix2, but prefix1 -> prefix2 -> postfix1 -> postfix2
    // (maybe patches are being applied twice?)
    // so we are making a flag to stop postfix2 (and any subsequent postfixN) from doing anything
    // prefix1(assign true) -> prefix2 -> postfix1(assign false) -> postfix2(does nothing)
    let mutable dangerouslyModifyingResults = false

    let Prefix(target: LocalTargetInfo, __instance: Verb_MeleeAttackDamage) = do dangerouslyModifyingResults <- true

    let Postfix(returned: IEnumerable<DamageInfo>, target: LocalTargetInfo, __instance: Verb_MeleeAttackDamage) =
        if Seq.isEmpty returned || not dangerouslyModifyingResults then
            returned
        else
            let damages =
                Option.ofObj __instance.EquipmentSource
                |> Option.bind compOfThing<Comp.Infusion>
                |> Option.map (fun comp ->
                    comp.ExtraDamages
                    |> Seq.filter (fun damage -> Rand.Chance damage.chance)
                    |> Seq.map (createDamageInfo __instance comp.parent target.Thing.Position))
                // need to prepend, as combat log is only associated with the last damage
                |> Option.map (fun damages -> Seq.append damages returned)
                |> Option.defaultValue returned

            do dangerouslyModifyingResults <- false

            seq damages
