namespace Infusion.OnHitWorkers

open Verse

open Infusion


type Sequence =
  inherit OnHitWorker

  val mutable value: ResizeArray<OnHitWorker>

  val mutable onMeleeCast: bool
  val mutable onMeleeImpact: bool
  val mutable onRangedCast: bool
  val mutable onRangedImpact: bool

  new() =
    { inherit OnHitWorker()

      value = null

      onMeleeCast = true
      onMeleeImpact = true
      onRangedCast = true
      onRangedImpact = true }

  override this.AfterAttack record =
    match record with
    | VerbCastedRecordMelee r when this.onMeleeCast ->
      for worker in this.value do
        if worker.chance >= 1.0f || Rand.Chance worker.chance then
          worker.AfterAttack record
    | VerbCastedRecordRanged r when this.onRangedCast ->
      for worker in this.value do
        if worker.chance >= 1.0f || Rand.Chance worker.chance then
          worker.AfterAttack record
    | _ -> ()

  override this.BulletHit record =
    if this.onRangedImpact then
      for worker in this.value do
        if worker.chance >= 1.0f || Rand.Chance worker.chance then
          worker.BulletHit record

  override this.ConfigErrors() =
    if isNull this.value then
      seq { "no value" }
    else
      Seq.empty

  override this.MeleeHit record =
    if this.onMeleeImpact then
      for worker in this.value do
        if worker.chance >= 1.0f || Rand.Chance worker.chance then
          worker.MeleeHit record

  override this.WearerDowned pawn apparel =
    Lib.runUntilFalseFrom 0 ((fun (worker: OnHitWorker) -> worker.WearerDowned pawn apparel), this.value)
