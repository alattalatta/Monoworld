namespace Infusion.OnHitWorkers

open Poet.Lib
open Poet.Lyric
open RimWorld
open Verse

open Infusion
open VerseTools


type ApplyHediff =
  inherit OnHitWorker

  val mutable bodySizeMatters: bool
  val mutable def: HediffDef
  val mutable inverseStatScaling: bool
  val mutable severityScaleBy: StatDef

  val mutable onMeleeCast: bool
  val mutable onMeleeImpact: bool

  new() =
    { bodySizeMatters = true
      def = null
      inverseStatScaling = false
      severityScaleBy = null

      onMeleeCast = true
      onMeleeImpact = true }

  override this.AfterAttack record =
    match record with
    | VerbCastedRecordMelee mr when this.onMeleeCast ->
      if this.selfCast then
        this.AddHediff mr.baseDamage mr.verb.CasterPawn
      else
        tryCast<Pawn> mr.target
        |> Option.iter (this.AddHediff mr.baseDamage)
    | VerbCastedRecordRanged mr ->
      if this.selfCast then
        this.AddHediff mr.baseDamage mr.verb.CasterPawn
      else
        tryCast<Pawn> mr.target
        |> Option.iter (this.AddHediff mr.baseDamage)
    | _ -> ()

  override this.BulletHit record =
    if this.selfCast then
      tryCast<Pawn> record.projectile.Launcher
      |> Option.iter (this.AddHediff record.baseDamage)
    else
      record.target
      |> Option.bind tryCast<Pawn>
      |> Option.iter (this.AddHediff record.baseDamage)

  override this.MeleeHit record =
    if this.onMeleeImpact then
      if this.selfCast then
        this.AddHediff record.baseDamage record.verb.CasterPawn
      else
        tryCast<Pawn> record.target
        |> Option.iter (this.AddHediff record.baseDamage)

  member private this.AddHediff baseDamage (pawn: Pawn) =
    if Pawn.isAliveAndWell pawn then
      let amount = baseDamage * this.amount
      let hediff = HediffMaker.MakeHediff(this.def, pawn)

      match Comp.ofHediff<HediffComp_Disappears> hediff with
      | Some comp -> comp.ticksToDisappear <- GenTicks.SecondsToTicks amount
      | _ -> hediff.Severity <- this.CalculateSeverity amount pawn

      pawn.health.AddHediff hediff

  member private this.CalculateSeverity amount (pawn: Pawn) =
    let statScale =
      Option.ofObj this.severityScaleBy
      |> Option.map pawn.GetStatValue
      |> Option.map (fun stat ->
        if this.inverseStatScaling then
          (max 0.0f (1.0f - stat))
        else
          stat)
      |> Option.defaultValue 1.0f

    let bodySizeScale =
      if this.bodySizeMatters then
        pawn.BodySize
      else
        1.0f

    amount * statScale / bodySizeScale / 100.0f
