module Infusion.Harmonize.VerbMeleeAttackDamage

open System.Collections.Generic

open HarmonyLib
open RimWorld
open Verse

open Infusion
open VerseInterop

[<HarmonyPatch(typeof<Verb_MeleeAttackDamage>, "DamageInfosToApply")>]
module RelevantGear =
    let private createDamageInfo
        (instance: Verb_MeleeAttackDamage)
        (source: ThingWithComps)
        (targetPos: IntVec3)
        (extraDamage: ExtraDamage)
        =
        let caster = instance.CasterPawn
        let pos = (targetPos - caster.Position).ToVector3()

        DamageInfo
            (extraDamage.def, extraDamage.amount, extraDamage.AdjustedArmorPenetration(instance, caster), -1.0f, caster,
             null, source.def)
        |> DamageInfo.setAngle pos
        |> DamageInfo.setBodyRegion BodyPartHeight.Undefined BodyPartDepth.Outside
        |> DamageInfo.setWeaponBodyPartGroup (instance.verbProps.AdjustedLinkedBodyPartsGroup instance.tool)

    let Postfix(returned: IEnumerable<DamageInfo>, target: LocalTargetInfo, __instance: Verb_MeleeAttackDamage) =
        if Seq.isEmpty returned then
            returned
        else
            Option.ofObj __instance.EquipmentSource
            |> Option.bind compOfThing<Comp.Infusion>
            |> Option.map (fun comp ->
                comp.Infusions
                |> Seq.filter Def.hasExtraMeleeDamage
                |> Seq.collect (fun def ->
                    def.extraMeleeDamages
                    |> Seq.filter (fun damage -> Rand.Chance damage.chance)
                    |> Seq.map (createDamageInfo __instance comp.parent target.Thing.Position)))
            |> Option.map (Seq.append returned)
            |> Option.defaultValue returned
