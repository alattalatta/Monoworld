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
        baseDamage
        (extraDamage: ExtraDamage)
        =
        let amount = baseDamage * extraDamage.amount

        let pos =
            (targetPos - instance.caster.Position).ToVector3()

        DamageInfo
            (extraDamage.def,
             Rand.Range(amount * 0.8f, amount * 1.2f),
             extraDamage.AdjustedArmorPenetration(instance, instance.CasterPawn),
             -1.0f,
             instance.caster,
             null,
             source.def)
        |> DamageInfo.setAngle pos
        |> DamageInfo.setBodyRegion BodyPartHeight.Undefined BodyPartDepth.Outside
        |> DamageInfo.setWeaponBodyPartGroup (instance.verbProps.AdjustedLinkedBodyPartsGroup instance.tool)

    // Adds new DamageInfo from infusions' extraDamages.
    let Postfix (returned: IEnumerable<DamageInfo>, target: LocalTargetInfo, __instance: Verb_MeleeAttackDamage) =
        let comp =
            Option.ofObj __instance.EquipmentSource
            |> Option.bind compOfThing<CompInfusion>

        let baseDamage =
            __instance.verbProps.AdjustedMeleeDamageAmount(__instance, __instance.CasterPawn)

        // explosions
        do comp
           |> Option.map (fun a ->
               (a.parent,
                a.ExtraExplosions
                |> Seq.filter (fun expl -> Rand.Chance expl.chance)))
           |> Option.iter (fun (equipment, expls) ->
               expls
               |> Seq.iter (fun expl ->
                   do GenExplosion.DoExplosion
                       (target.Cell,
                        __instance.caster.Map,
                        expl.radius,
                        expl.def,
                        equipment,
                        int (baseDamage * (float32 expl.amount)))))

        // damages
        if Seq.isEmpty returned then
            returned
        else
            let damages =
                comp
                |> Option.map (fun comp ->
                    comp.ExtraDamages
                    |> Seq.filter (fun damage -> Rand.Chance damage.chance)
                    |> Seq.map (createDamageInfo __instance comp.parent target.Thing.Position baseDamage))
                |> Option.defaultValue Seq.empty

            // need to prepend, as combat log is only associated with the last damage
            Seq.append damages returned
