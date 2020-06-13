module Infusion.Harmonize.VerbMeleeAttackDamage

open HarmonyLib
open Poet.Lyric
open RimWorld
open Verse

open Infusion
open Infusion.OnHitWorkers


[<HarmonyPatch(typeof<Verb_MeleeAttackDamage>, "ApplyMeleeDamageToTarget")>]
module ApplyMeleeDamageToTarget =
    let Postfix (target: LocalTargetInfo, __instance: Verb_MeleeAttackDamage) =
        let comp =
            Option.ofObj __instance.EquipmentSource
            // really this should only apply to melee weapons
            |> Option.filter (fun e -> e.def.IsMeleeWeapon)
            |> Option.bind Comp.ofThing<CompInfusion>

        let baseDamage =
            __instance.verbProps.AdjustedMeleeDamageAmount(__instance, __instance.CasterPawn)

        do comp
           |> Option.map (fun c -> (c, c.OnHits))
           |> Option.iter (fun (c, onHits) ->
               onHits
               |> List.filter (fun onHit -> Rand.Chance onHit.chance)
               |> List.iter (fun onHit ->
                   do onHit.MeleeHit
                       { baseDamage = baseDamage
                         source = c.parent
                         target = target.Thing
                         verb = __instance }))
