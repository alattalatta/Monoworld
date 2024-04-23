module Infusion.Harmonize.VerbMeleeAttackDamage

open HarmonyLib
open RimWorld
open Verse

open Infusion


[<HarmonyPatch(typeof<Verb_MeleeAttackDamage>, "ApplyMeleeDamageToTarget")>]
// applies on hit effects
module ApplyMeleeDamageToTarget =
  let Postfix (target: LocalTargetInfo, __instance: Verb_MeleeAttackDamage) =
    let baseDamage =
      __instance.verbProps.AdjustedMeleeDamageAmount(__instance, __instance.CasterPawn)

    // the weapon
    Option.ofObj __instance.EquipmentSource
    // really, this should only apply to melee weapons
    |> Option.filter (fun c -> c.def.IsMeleeWeapon)
    |> Option.bind CompInfusion.forOnHitWorkers
    |> Option.iter (fun (workers, comp) ->
      workers
      |> List.iter (fun onHit ->
        onHit.MeleeHit
          { baseDamage = baseDamage
            source = comp.parent
            target = target.Thing
            verb = __instance }))
