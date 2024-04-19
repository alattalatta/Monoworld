module Infusion.Harmonize.Bullet

open HarmonyLib
open Poet.Lib
open Poet.Lyric
open RimWorld
open Verse

open Infusion


[<HarmonyPatch(typeof<Bullet>, "Impact")>]
// applies on hit effects
module Impact =
  let Prefix (hitThing: Thing, __instance: Bullet, __state: outref<Map>) = do __state <- __instance.Map

  let Postfix (hitThing: Thing, __instance: Bullet, __state: Map) =
    let baseDamage = float32 __instance.DamageAmount

    tryCast<Pawn> __instance.Launcher
    |> Option.bind Pawn.getPrimaryEquipment
    // get the primary's infusions
    |> Option.bind Thing.getComp<CompInfusion>
    |> Option.filter (fun comp -> comp.EffectsEnabled)
    |> Option.iter (fun comp ->
      // execute OnHitWorkers
      comp.OnHits
      |> List.filter (fun onHit -> Rand.Chance onHit.chance)
      |> List.iter (fun onHit ->
        do
          onHit.BulletHit
            { baseDamage = baseDamage
              map = __state
              projectile = __instance
              target = Option.ofObj hitThing
              sourceDef = __instance.EquipmentDef }))
