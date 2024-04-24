namespace Infusion.OnHitWorkers

open Poet.Lib
open RimWorld
open Verse

open Infusion
open VerseTools


type SpawnBlood() =
  inherit OnHitWorker()

  override this.BulletHit record =
    record.target
    |> Option.bind tryCast<Pawn>
    |> Option.filter Pawn.isAliveAndWell
    |> Option.iter (fun t ->
      FilthMaker.TryMakeFilth(t.Position, t.Map, t.RaceProps.BloodDef, t.LabelIndefinite())
      |> ignore)

  override this.MeleeHit record =
    tryCast<Pawn> record.target
    |> Option.filter Pawn.isAliveAndWell
    |> Option.iter (fun t ->
      FilthMaker.TryMakeFilth(t.Position, t.Map, t.RaceProps.BloodDef, t.LabelIndefinite())
      |> ignore)
