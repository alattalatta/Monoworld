module Infusion.Harmonize.Bullet

open HarmonyLib
open Poet.Lib
open Poet.Lyric
open RimWorld
open Verse

open Infusion
open Infusion.OnHitWorkers


[<HarmonyPatch(typeof<Bullet>, "Impact")>]
module Impact =
  let Prefix (hitThing: Thing, __instance: Bullet, __state: outref<Map>) = do __state <- __instance.Map

  let Postfix (hitThing: Thing, __instance: Bullet, __state: Map) =
    let comp =
      tryCast<Pawn> __instance.Launcher
      |> Option.bind Pawn.getEquipments
      |> Option.bind (Seq.tryFind (fun e -> e.def.equipmentType = EquipmentType.Primary))
      |> Option.bind Thing.getComp<CompInfusion>

    let baseDamage = float32 __instance.DamageAmount

    do
      comp
      |> Option.filter (fun c -> c.EffectsEnabled)
      |> Option.iter (fun c ->
        c.OnHits
        |> List.filter (fun onHit -> Rand.Chance onHit.chance)
        |> List.iter (fun onHit ->
          do
            onHit.BulletHit
              { baseDamage = baseDamage
                map = __state
                projectile = __instance
                target = Option.ofObj hitThing
                sourceDef = __instance.EquipmentDef }))
