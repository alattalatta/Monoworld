namespace Infusion.OnHitWorkers

open Poet.Lyric
open RimWorld
open UnityEngine
open Verse

open Infusion
open VerseTools


type Preservation() =
  inherit OnHitWorker()

  static let mutable hediff: HediffDef = null
  static let mutable rulePack: RulePackDef = null

  override this.WearerDowned pawn apparel =
    if hediff = null then
      hediff <- (GenDefDatabase.GetDef(typeof<HediffDef>, "Infusion_Preservation") :?> HediffDef)

    if rulePack = null then
      rulePack <- (GenDefDatabase.GetDef(typeof<RulePackDef>, "Infusion_Preservation") :?> RulePackDef)

    if Pawn.isAliveAndWell pawn then
      let prevHediff = pawn.health.hediffSet.GetFirstHediffOfDef hediff

      let hediff =
        if prevHediff = null then
          let newHediff = HediffMaker.MakeHediff(hediff, pawn)
          pawn.health.AddHediff newHediff
          newHediff
        else
          Comp.ofHediff<HediffComp_Disappears> prevHediff
          |> Option.iter (fun comp ->
            comp.ticksToDisappear <-
              comp.ticksToDisappear
              + (comp.props :?> HediffCompProperties_Disappears)
                .disappearsAfterTicks
                .min)

          prevHediff

      let logEntry = new BattleLogEntry_ItemUsed(pawn, pawn, apparel.def, rulePack)
      hediff.combatLogEntry <- new WeakReference<LogEntry>(logEntry)
      hediff.combatLogText <- logEntry.ToGameStringFromPOV(null)
      Find.BattleLog.Add(logEntry)

      MoteMaker.ThrowText(
        new Vector3((float32 pawn.Position.x) + 1f, (float32 pawn.Position.y), (float32 pawn.Position.z) + 1f),
        pawn.Map,
        (ResourceBank.Strings.OnHitWorkers.sacrificed apparel.Label),
        Color(1.0f, 0.5f, 0.0f)
      )

      apparel.Destroy()

      false
    else
      true
