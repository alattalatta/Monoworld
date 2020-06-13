namespace Infusion.OnHitWorkers

open Verse


type Sequence =
    inherit OnHitWorker

    val mutable value: ResizeArray<OnHitWorker>

    new() =
        { inherit OnHitWorker()

          value = null }

    member this.Value = Option.ofObj this.value

    override this.MeleeHit record =
        this.Value
        |> Option.iter (fun sequence ->
            for worker in sequence do
                if Rand.Chance worker.chance then do worker.MeleeHit record)

    override this.BulletHit record =
        this.Value
        |> Option.iter (fun sequence ->
            for worker in sequence do
                if Rand.Chance worker.chance then do worker.BulletHit record)
