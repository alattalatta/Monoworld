namespace Infusion.OnHitWorkers

open Verse


type Sequence =
  inherit OnHitWorker

  val mutable value: ResizeArray<OnHitWorker>

  new() =
    { inherit OnHitWorker()

      value = null }

  override this.BulletHit record =
    for worker in this.value do
      if Rand.Chance worker.chance then
        do worker.BulletHit record

  override this.ConfigErrors() =
    if isNull this.value then
      seq { "no value" }
    else
      Seq.empty

  override this.MeleeHit record =
    for worker in this.value do
      if Rand.Chance worker.chance then
        do worker.MeleeHit record
