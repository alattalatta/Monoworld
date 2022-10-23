namespace Infusion.OnHitWorkers

open Poet.Lib
open RimWorld
open Verse

open Infusion
open VerseTools


type SpawnBlood() =
  inherit OnHitWorker()

  override this.MeleeHit record =
    do
      tryCast<Pawn> record.target
      |> Option.filter Pawn.isAliveAndWell
      |> Option.iter (fun t ->
        do
          FilthMaker.TryMakeFilth(t.Position, t.Map, t.RaceProps.BloodDef, t.LabelIndefinite())
          |> ignore)

  override this.BulletHit record =
    do
      record.target
      |> Option.bind tryCast<Pawn>
      |> Option.filter Pawn.isAliveAndWell
      |> Option.iter (fun t ->
        do
          FilthMaker.TryMakeFilth(t.Position, t.Map, t.RaceProps.BloodDef, t.LabelIndefinite())
          |> ignore)
