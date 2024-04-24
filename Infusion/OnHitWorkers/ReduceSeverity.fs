namespace Infusion.OnHitWorkers

open Poet.Lib
open RimWorld
open Verse

open Infusion
open VerseTools


// [todo] Group as HediffBase
type ReduceSeverity =
  inherit OnHitWorker

  val mutable bodySizeMatters: bool
  val mutable def: HediffDef
  val mutable severityScaleBy: StatDef

  val mutable onMeleeCast: bool
  val mutable onMeleeImpact: bool
  val mutable onRangedCast: bool
  val mutable onRangedImpact: bool

  new() =
    { bodySizeMatters = true
      def = null
      severityScaleBy = null

      onMeleeCast = true
      onMeleeImpact = true
      onRangedCast = true
      onRangedImpact = true }

  override this.AfterAttack record =
    match record with
    | VerbCastedRecordMelee r when this.onMeleeCast -> this.ReduceSeverityBy r.baseDamage r.verb.Caster
    | VerbCastedRecordRanged r when this.onRangedCast -> this.ReduceSeverityBy r.baseDamage r.verb.Caster
    | _ -> ()

  override this.BulletHit record =
    this.ReduceSeverityBy record.baseDamage record.projectile.Launcher

  override this.MeleeHit record =
    this.ReduceSeverityBy record.baseDamage record.verb.CasterPawn

  member private this.CalculateSeverity amount (pawn: Pawn) =
    let statScale =
      Option.ofObj this.severityScaleBy
      |> Option.map pawn.GetStatValue
      |> Option.defaultValue 1.0f

    let bodySizeScale =
      if this.bodySizeMatters then
        pawn.BodySize
      else
        1.0f

    amount * statScale / bodySizeScale / 100.0f

  member private this.ReduceSeverityBy baseDamage (caster: Thing) =
    tryCast<Pawn> caster
    |> Option.filter Pawn.isAliveAndWell
    |> Option.iter (fun pawn ->
      let amount = baseDamage * this.amount

      Option.ofObj (pawn.health.hediffSet.GetFirstHediffOfDef this.def)
      |> Option.iter (fun hediff -> hediff.Heal(this.CalculateSeverity amount pawn)))
