module Infusion.Harmonize.VerbMeleeAttackDamage

open HarmonyLib
open Poet.Lyric
open RimWorld
open Verse

open Infusion
open Infusion.OnHitWorkers


[<HarmonyPatch(typeof<Verb_MeleeAttackDamage>, "ApplyMeleeDamageToTarget")>]
// applies on hit effects
module ApplyMeleeDamageToTarget =
  let Postfix (target: LocalTargetInfo, __instance: Verb_MeleeAttackDamage) =
    let baseDamage =
      __instance.verbProps.AdjustedMeleeDamageAmount(__instance, __instance.CasterPawn)
      
    // the weapon
    Option.ofObj __instance.EquipmentSource
    // get its infusions
    |> Option.bind Comp.ofThing<CompInfusion>
    // really, this should only apply to melee weapons
    |> Option.filter (fun c -> c.parent.def.IsMeleeWeapon && c.EffectsEnabled)
    |> Option.iter (fun c ->
      // execute OnHitWorkers
      c.OnHits
      |> List.filter (fun onHit -> Rand.Chance onHit.chance)
      |> List.iter (fun onHit ->
        do
          onHit.MeleeHit
            { baseDamage = baseDamage
              source = c.parent
              target = target.Thing
              verb = __instance }))
