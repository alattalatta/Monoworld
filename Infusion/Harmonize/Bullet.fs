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
    |> Option.bind CompInfusion.forOnHitWorkers
    |> Option.iter (fun (workers, comp) ->
      for worker in workers do
        worker.BulletHit
          { baseDamage = baseDamage
            map = __state
            projectile = __instance
            source = comp.parent
            target = Option.ofObj hitThing })
