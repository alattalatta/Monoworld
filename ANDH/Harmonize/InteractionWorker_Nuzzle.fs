module ANDH.InteractionWorker_Nuzzle

open HarmonyLib
open RimWorld
open Verse


[<HarmonyPatch(typeof<InteractionWorker_Nuzzle>, "AddNuzzledThought")>]
module AddNuzzledThought =
  let Postfix (initiator: Pawn, recipient: Pawn) =
    if MetalhorrorUtility.IsInfected recipient then
      let owner =
        Option.ofObj (initiator.relations)
        |> Option.bind (fun rel -> Option.ofObj (rel.GetFirstDirectRelationPawn(PawnRelationDefOf.Bond)))

      let detectionChance =
        if owner
           |> Option.filter (fun o -> o = recipient)
           |> Option.isSome then
          Settings.BondedDetectionChance.handle
        else
          Settings.DetectionChance.handle

      if Rand.Chance detectionChance then
        let initiatorArg = initiator.Named("PAWN")
        let recipientArg = recipient.Named("INFECTED")

        let noticedArg =
          owner
          // the owner should not be the infected one
          |> Option.filter (fun owner -> owner <> recipient)
          |> Option.orElseWith (fun () ->
            // within current map...
            Option.ofObj initiator.Map
            // which is a player colony,
            |> Option.filter (fun map -> map.IsPlayerHome)
            |> Option.bind (fun map ->
              // pick one who is...
              map.mapPawns.FreeColonistsSpawned
              |> Seq.filter (fun pawn ->
                // not dead, not downed, not in a mental state and not infected
                not (
                  pawn.DeadOrDowned
                  || pawn.InMentalState
                  || MetalhorrorUtility.IsInfected(pawn)
                )
                // must be able to see or hear
                && (pawn.health.capacities.CapableOf(PawnCapacityDefOf.Hearing) || pawn.health.capacities.CapableOf(PawnCapacityDefOf.Sight))
                // within certain radius
                && pawn.Position.DistanceTo(recipient.Position) <= Settings.DetectionRadius.handle.Value
              )
              |> Seq.tryHead))
          |> Option.map (fun noticed -> noticed.Named("NOTICED"))
          
        // don't do anything when nobody noticed
        noticedArg
        |> Option.iter (fun noticed ->
          let emergingReason =
            ResourceBank.Strings.metalhorrorReasonNuzzled initiatorArg recipientArg noticed

          let subtleMessageBody =
            ResourceBank.Strings.nuzzleDetectedDesc initiatorArg recipientArg noticed

          let subtleMessageExplainer =
            ResourceBank.Strings.metalhorrorExplainer initiatorArg recipientArg

          let emergenceChance =
            if Settings.CanTriggerEmerging.handle.Value then
              Settings.EmergenceChance.handle.Value
            else
              0f

          MetalhorrorUtility.Detect(
            recipient,
            emergingReason,
            subtleMessageBody
            + "\n\n"
            + subtleMessageExplainer,
            emergenceChance
          ))
