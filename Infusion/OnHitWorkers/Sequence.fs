namespace Infusion.OnHitWorkers

open Verse

open Infusion


type Sequence =
  inherit OnHitWorker

  val mutable value: ResizeArray<OnHitWorker>

  new() =
    { inherit OnHitWorker()

      value = null }

  override this.BulletHit record =
    for worker in this.value do
      if worker.chance >= 1.0f || Rand.Chance worker.chance then
        do worker.BulletHit record

  override this.ConfigErrors() =
    if isNull this.value then
      seq { "no value" }
    else
      Seq.empty

  override this.MeleeHit record =
    for worker in this.value do
      if worker.chance >= 1.0f || Rand.Chance worker.chance then
        do worker.MeleeHit record

  override this.WearerDowned pawn apparel =
    Lib.runUntilFalseFrom 0 ((fun (worker: OnHitWorker) -> worker.WearerDowned pawn apparel), this.value)
