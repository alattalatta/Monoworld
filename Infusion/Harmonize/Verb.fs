module Infusion.Harmonize.Verb

open HarmonyLib
open Verse

open Infusion


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
          onHit.AfterAttack
            { source = comp.parent
              target = currentTarget.Thing
              verb = __instance }))
