module Infusion.Harmonize.Bullet

open HarmonyLib
open RimWorld
open Verse

open Infusion
open VerseInterop

[<HarmonyPatch(typeof<Bullet>, "Impact")>]
module Impact =
    let mutable hasReportedError = false

    let Prefix (hitThing: Thing, __instance: Bullet, __state: outref<Map>) = do __state <- __instance.Map

    let Postfix (hitThing: Thing, __instance: Bullet, __state: Map) =
        if not (isNull hitThing) then
            let primaryEquipment =
                match __instance.Launcher with
                | :? Pawn as p -> equipmentsOfPawn p
                | _ -> None
                |> Option.bind (Seq.tryFind (fun e -> e.def.equipmentType = EquipmentType.Primary))

            // explosions
            do primaryEquipment
               |> Option.bind compOfThing<CompInfusion>
               |> Option.map (fun comp ->
                   (comp,
                    comp.ExtraDamages
                    |> Seq.filter (fun exdam -> Rand.Chance exdam.chance),
                    comp.ExtraExplosions
                    |> Seq.filter (fun expl -> Rand.Chance expl.chance)))
               |> Option.iter (fun (comp, exdams, expls) ->
                   let direction =
                       __instance.ExactPosition.AngleToFlat(__instance.ExactPosition)

                   // protection against unexpected reflection errors
                   let intendedTarget =
                       try
                           Reflectors.Projectile.intendedTargetOf __instance
                           |> Some
                       with
                       // [todo] Remove duplicated
                       | :? System.ArgumentNullException as ex ->
                           if not hasReportedError then
                               do Log.Warning
                                   (sprintf "[Infusion 2] Reflection against Bullet#intendedTarget failed. Please report this with your mods list.\n%A"
                                        ex)
                               do hasReportedError <- true
                           None
                       | ex ->
                           if not hasReportedError then
                               do Log.Warning
                                   (sprintf "[Infusion 2] Unknown error occured while getting Bullet#intendedTarget. Please report this with your mods list.\n%A"
                                        ex)
                               do hasReportedError <- true
                           None

                   // damages
                   exdams
                   |> Seq.iter (fun exdam ->
                       let ap =
                           if exdam.armorPenetration >= 0.0f then
                               exdam.armorPenetration
                           else
                               __instance.ArmorPenetration

                       let dinfo =
                           DamageInfo
                               (exdam.def,
                                exdam.amount * float32 __instance.DamageAmount,
                                ap,
                                __instance.ExactRotation.eulerAngles.y,
                                __instance.Launcher,
                                weapon = comp.parent.def)

                       do hitThing.TakeDamage dinfo |> ignore)

                   // explosions
                   expls
                   |> Seq.iter (fun expl ->
                       GenExplosion.DoExplosion
                           (__instance.Position,
                            __state,
                            1.0f,
                            expl.def,
                            __instance.Launcher,
                            int (expl.amount * float32 __instance.DamageAmount),
                            intendedTarget =
                                (intendedTarget
                                 |> Option.map (fun t -> t.Thing)
                                 |> Option.defaultValue (null)),
                            weapon = comp.parent.def,
                            projectile = __instance.def,
                            direction = new System.Nullable<float32>(direction))))
