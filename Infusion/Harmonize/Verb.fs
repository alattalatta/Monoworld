module Infusion.Harmonize.Verb

open HarmonyLib
open RimWorld
open Verse

open Infusion
open VerseTools


module private Verb =
  let burstShotsLeftFi = AccessTools.Field(typeof<Verb>, "burstShotsLeft")
  let currentTargetFi = AccessTools.Field(typeof<Verb>, "currentTarget")


[<HarmonyPatch(typeof<Verb>, "TryCastNextBurstShot")>]
module TryCastNextBurstShot =
  let Postfix (__instance: Verb) =
    if (Verb.burstShotsLeftFi.GetValue(__instance) :?> int) = 0 then
      let currentTarget = Verb.currentTargetFi.GetValue(__instance) :?> LocalTargetInfo

      Option.ofObj __instance.EquipmentCompSource
      |> Option.map (fun c -> c.parent)
      |> Option.bind CompInfusion.forOnHitWorkers
      |> Option.iter (fun (workers, comp) ->
        workers
        |> List.iter (fun onHit ->
          let melee =
            __instance
              .GetType()
              .IsSubclassOf(typeof<Verb_MeleeAttack>)

          let data =
            {| baseDamage =
                if melee then
                  Verb.getAdjustedMeleeDamage __instance
                else
                  Option.ofObj __instance.verbProps.defaultProjectile
                  |> Option.bind (fun x -> Option.ofObj x.projectile)
                  |> Option.map (fun projectile -> projectile.GetDamageAmount(comp.parent) |> float32)
                  |> Option.map (fun dam ->
                    dam
                    * ((float32 (__instance.verbProps.burstShotCount) / 5f)
                       |> round
                       |> max 1f))
                  |> Option.defaultValue 0f

               source = comp.parent
               target = currentTarget.Thing
               verb = __instance |}

          if data.baseDamage > 0f then
            let record =
              if melee then
                VerbCastedRecordMelee
              else
                VerbCastedRecordRanged

            onHit.AfterAttack(record data)))
