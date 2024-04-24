namespace Infusion.OnHitWorkers

open Poet.Lib
open Verse

open Infusion
open VerseTools


// [todo] Associate with battle logs
type HealCaster =
  inherit OnHitWorker

  val mutable onMeleeCast: bool
  val mutable onMeleeImpact: bool
  val mutable onRangedCast: bool
  val mutable onRangedImpact: bool

  new() =
    { inherit OnHitWorker()

      onMeleeCast = true
      onMeleeImpact = true
      onRangedCast = true
      onRangedImpact = true }

  override this.AfterAttack record =
    match record with
    | VerbCastedRecordMelee r when this.onMeleeCast -> this.HealRandomInjury r.baseDamage r.verb.Caster
    | VerbCastedRecordRanged r when this.onRangedCast -> this.HealRandomInjury r.baseDamage r.verb.Caster
    | _ -> ()

  override this.BulletHit record =
    if this.onRangedImpact then
      this.HealRandomInjury record.baseDamage record.projectile.Launcher

  override this.MeleeHit record =
    if this.onMeleeImpact then
      this.HealRandomInjury record.baseDamage record.verb.CasterPawn

  member private this.HealRandomInjury baseDamage (caster: Thing) =
    tryCast<Pawn> caster
    |> Option.filter Pawn.isAliveAndWell
    |> Option.iter (fun pawn ->
      let amount = baseDamage * this.amount
      let hediffSet = pawn.health.hediffSet

      if hediffSet.HasNaturallyHealingInjury() then
        let injuries = ResizeArray<Hediff_Injury>()
        hediffSet.GetHediffs<Hediff_Injury>(ref injuries, (fun hediff -> hediff.CanHealNaturally()))

        injuries
          .RandomElement()
          .Heal(amount * pawn.HealthScale))
