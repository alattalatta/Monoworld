module Infusion.Harmonize.VerbMeleeAttackDamage

open HarmonyLib
open RimWorld
open Verse

open Infusion
open VerseTools


[<HarmonyPatch(typeof<Verb_MeleeAttackDamage>, "ApplyMeleeDamageToTarget")>]
// applies on hit effects
module ApplyMeleeDamageToTarget =
  let Postfix (target: LocalTargetInfo, __instance: Verb_MeleeAttackDamage) =
    // the weapon
    Option.ofObj __instance.EquipmentSource
    // really, this should only apply to melee weapons
    |> Option.filter (fun c -> c.def.IsMeleeWeapon)
    |> Option.bind CompInfusion.forOnHitWorkers
    |> Option.iter (fun (workers, comp) ->
      workers
      |> List.iter (fun onHit ->
        onHit.MeleeHit
          {| baseDamage = Verb.getAdjustedMeleeDamage __instance
             source = comp.parent
             target = target.Thing
             verb = __instance |}))
