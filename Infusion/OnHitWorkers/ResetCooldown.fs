namespace Infusion.OnHitWorkers

open Poet.Lib
open Verse

open Infusion


type ResetCooldown() =
  inherit OnHitWorker()

  override this.AfterAttack record =
    tryCast<Pawn> record.verb.Caster
    |> Option.bind (fun pawn -> Option.ofObj pawn.stances.curStance)
    |> Option.bind tryCast<Stance_Cooldown>
    |> Option.iter (fun stance ->
      stance.ticksLeft <- 0
    )
