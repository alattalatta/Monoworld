namespace Infusion.OnHitWorkers

open Poet.Lib
open Verse

open Infusion


type ResetCooldown =
  inherit OnHitWorker

  val mutable onMeleeCast: bool
  val mutable onMeleeImpact: bool

  new() =
    { inherit OnHitWorker()

      onMeleeCast = true
      onMeleeImpact = true }

  override this.AfterAttack record =
    match record with
    | VerbCastedRecordMelee r when this.onMeleeCast -> this.ResetCooldown r.verb.Caster
    | VerbCastedRecordRanged r -> this.ResetCooldown r.verb.Caster
    | _ -> ()

  override this.MeleeHit record =
    if this.onMeleeImpact then
      this.ResetCooldown record.verb.Caster

  member private this.ResetCooldown caster =
    tryCast<Pawn> caster
    |> Option.bind (fun pawn -> Option.ofObj pawn.stances.curStance)
    |> Option.bind tryCast<Stance_Cooldown>
    |> Option.iter (fun stance -> stance.ticksLeft <- 0)
