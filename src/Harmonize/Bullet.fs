module Infusion.Harmonize.Bullet

open HarmonyLib
open RimWorld
open Verse

open Infusion
open VerseInterop

[<HarmonyPatch(typeof<Bullet>, "Impact")>]
module Impact =
    let Prefix (hitThing: Thing, __instance: Bullet, __state: outref<Map>) = do __state <- __instance.Map

    let Postfix (hitThing: Thing, __instance: Bullet, __state: Map) =
        let primaryEquipment =
            match __instance.Launcher with
            | :? Pawn as p -> equipmentsOfPawn p
            | _ -> None
            |> Option.bind (Seq.tryFind (fun e -> e.def.equipmentType = EquipmentType.Primary))

        do primaryEquipment
           |> Option.bind compOfThing<Comp.Infusion>
           |> Option.map (fun comp ->
               (comp,
                comp.ExtraExplosions
                |> Seq.filter (fun expl -> Rand.Chance expl.chance)))
           |> Option.iter (fun (comp, expls) ->
               let direction =
                   __instance.ExactPosition.AngleToFlat(__instance.ExactPosition)

               let intendedTarget =
                   Reflectors.Projectile.intendedTargetOf __instance

               expls
               |> (Seq.iter (fun expl ->
                       GenExplosion.DoExplosion
                           (__instance.Position,
                            __state,
                            1.0f,
                            expl.def,
                            __instance.Launcher,
                            expl.amount,
                            intendedTarget = intendedTarget.Thing,
                            weapon = comp.parent.def,
                            projectile = __instance.def,
                            direction = new System.Nullable<float32>(direction)))))
