module ANDH.InteractionWorker_Nuzzle

open HarmonyLib
open RimWorld
open Verse

#if DEBUG
open Poet.Lib
#endif


let private chooseNoticer weighter (candidates: Pawn seq, recipient: Pawn) =
  // pick one who is...
  let filtered =
    candidates
    |> Seq.filter (fun pawn ->
      // not dead, not downed, not in a mental state and not infected
      not (
        pawn.DeadOrDowned
        || pawn.InMentalState
        || MetalhorrorUtility.IsInfected(pawn)
      )
      // must be able to see or hear
      && (pawn.health.capacities.CapableOf(PawnCapacityDefOf.Hearing)
          || pawn.health.capacities.CapableOf(PawnCapacityDefOf.Sight))
      // within certain radius
      && pawn.Position.DistanceTo(recipient.Position)
         <= Settings.DetectionRadius.handle.Value)

  let list = new System.Collections.Generic.List<Pawn>(filtered)

  if list.Count > 0 then
    Some(list.RandomElementByWeight weighter)
  else
    None


let private doDetection (initiator: Pawn, recipient: Pawn, noticed: Pawn, recipientBonded: bool, falseAlarm: bool) =
  #if DEBUG
  log "initiator: %A\nrecipient: %A\nnoticed: %A" initiator recipient noticed
  #endif

  let initiatorArg = initiator.Named("PAWN")
  let recipientArg = initiator.Named("INFECTED")
  let noticedArg = noticed.Named("NOTICED")

  let emergingReason =
    ResourceBank.Strings.metalhorrorReasonNuzzled initiatorArg recipientArg noticedArg

  let subtleMessageBody =
    let resource =
      if Settings.StressedFalseAlarm.handle.Value > 0.0f then
        if recipientBonded then
          ResourceBank.Strings.nuzzleDetectedDescFallible
        else
          ResourceBank.Strings.nuzzleDetectedDescFallible
      else if recipientBonded then
        ResourceBank.Strings.nuzzleDetectedDesc
      else
        ResourceBank.Strings.nuzzleDetectedDesc

    resource initiatorArg recipientArg noticedArg

  let subtleMessageExplainer =
    ResourceBank.Strings.metalhorrorExplainer initiatorArg recipientArg

  let emergenceChance =
    if Settings.CanTriggerEmerging.handle.Value then
      Settings.EmergenceChance.handle.Value
    else
      0f

  let subtleMessage = 
      subtleMessageBody
      + "\n\n"
      + subtleMessageExplainer

  if falseAlarm then
    if not (recipient.health.hediffSet.HasHediff(ResourceBank.Defs.ANDH_MetalhorrorImplant_FalseAlarm)) then
      recipient.health.AddHediff(ResourceBank.Defs.ANDH_MetalhorrorImplant_FalseAlarm) |> ignore
      Find.LetterStack.ReceiveLetter("MetalhorrorDetected".Translate(), subtleMessage, LetterDefOf.ThreatSmall)
  else
    MetalhorrorUtility.Detect(
      recipient,
      emergingReason,
      subtleMessage,
      emergenceChance
    )


[<HarmonyPatch(typeof<InteractionWorker_Nuzzle>, "Interacted")>]
module Interacted =
  let Prefix
    (
      initiator: Pawn,
      recipient: Pawn,
      extraSentencePacks: System.Collections.Generic.List<RulePackDef>,
      letterText: outref<string>,
      letterLabel: outref<string>,
      letterDef: outref<LetterDef>,
      lookTargets: outref<LookTargets>
    ) =
    let initiatorIsAnimal = initiator.NonHumanlikeOrWildMan()
    let recipientInfected = MetalhorrorUtility.IsInfected recipient

    let bonded =
      Option.ofObj (initiator.relations)
      |> Option.bind (fun rel -> Option.ofObj (rel.GetFirstDirectRelationPawn(PawnRelationDefOf.Bond)))

    let recipientIsBonded =
      bonded
      |> Option.filter (fun pb -> pb = recipient)
      |> Option.isSome

#if DEBUG
    log "hasSeenGrayFlesh: %A\nrecipientInfected: %A" Find.Anomaly.hasSeenGrayFlesh recipientInfected
#endif

    if not initiatorIsAnimal then
      true
    // false alarm
    else if Find.Anomaly.hasSeenGrayFlesh
            && not recipientInfected
            && Rand.Chance Settings.StressedFalseAlarm.handle then
#if DEBUG
      log "doing false alarm"
#endif
      let candidates =
        Option.ofObj initiator.Map
        |> Option.filter (fun m -> m.IsPlayerHome)
        |> Option.map (fun m ->
          seq m.mapPawns.FreeColonistsSpawned
          |> Seq.filter (fun p ->
            let mb = p.mindState.mentalBreaker
            mb.CurMood <= mb.BreakThresholdMinor))
        |> Option.defaultValue Seq.empty

#if DEBUG
      log "candidates: %A" candidates
#endif

      let weighter =
        (fun (p: Pawn) ->
          // prefer higher animal skill
          let weightBySkill =
            p.skills.GetSkill(SkillDefOf.Animals).Level
            |> float32
            |> (*) 2f

          let mb = p.mindState.mentalBreaker

          let weightByStress =
            if mb.BreakExtremeIsImminent then 9f
            elif mb.BreakMajorIsImminent then 3f
            else 1f

          weightBySkill * weightByStress)

      let noticed = chooseNoticer weighter (candidates, recipient)

#if DEBUG
      log "%A: noticed (false) by %A" initiator.Label noticed
#endif

      // don't do anything when nobody "noticed"
      noticed
      |> Option.iter (fun noticed -> doDetection (initiator, recipient, noticed, recipientIsBonded, true))

      // do not prevent interaction, the initiator actually nuzzled
      true
    // real alarm
    else if recipientInfected then
      let detectionChance =
        if bonded
           |> Option.filter (fun o -> o = recipient)
           |> Option.isSome then
#if DEBUG
          log "%A: nuzzled bonded one" initiator.Label
#endif
          Settings.BondedDetectionChance.handle
        else
#if DEBUG
          log "%A: nuzzled random person" initiator.Label
#endif
          Settings.DetectionChance.handle

      if Rand.Chance detectionChance then
        let candidates =
          Option.ofObj initiator.Map
          |> Option.filter (fun m -> m.IsPlayerHome)
          |> Option.map (fun m -> seq m.mapPawns.FreeColonistsSpawned)
          |> Option.defaultValue (Seq.empty)

#if DEBUG
        log "candidates: %A" candidates
#endif

        let weighter =
          (fun (p: Pawn) ->
            // prefer higher animal skill
            let weightBySkill =
              p.skills.GetSkill(SkillDefOf.Animals).Level
              |> float32
              |> (*) 2f

            // prefer bonded colonist
            match bonded with
            | Some pb ->
              if p = pb then
                weightBySkill * 5f
              else
                weightBySkill
            | None -> weightBySkill)

        let noticed = chooseNoticer weighter (candidates, recipient)

#if DEBUG
        log "%A: noticed by %A" initiator.Label noticed
#endif

        // don't do anything when nobody noticed
        noticed
        |> Option.iter (fun noticed -> doDetection (initiator, recipient, noticed, recipientIsBonded, false))

        // prevent interaction as the initiator didn't really nuzzle
        false
      else
        true
    else
      true
