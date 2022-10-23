namespace Infusion.OnHitWorkers

open Poet.Lyric
open Verse

open Infusion
open VerseTools


// [todo] Associate with battle logs
type ApplyDamage() =
  inherit DamageBase()

  override this.MeleeHit record =
    if Pawn.isAliveAndWell record.target then
      do
        this.CreateMeleeDamageInfo record
        |> record.target.TakeDamage
        |> ignore

  override this.BulletHit record =
    do
      record.target
      |> Option.filter Pawn.isAliveAndWell
      |> Option.iter (fun t ->
        do
          this.CreateRangedDamageInfo record
          |> t.TakeDamage
          |> ignore)

  member private this.CreateMeleeDamageInfo(record: MeleeHitRecord) =
    let amount = record.baseDamage * this.amount

    let direction =
      (record.target.Position
       - record.verb.caster.Position)
        .ToVector3()

    DamageInfo(
      this.def,
      Rand.Range(amount * 0.8f, amount * 1.2f),
      this.MeleeArmorPen record,
      angle = -1.0f,
      instigator = record.verb.caster,
      weapon = record.source.def
    )
    |> DamageInfo.setAngle direction
    |> DamageInfo.setBodyRegion BodyPartHeight.Undefined BodyPartDepth.Outside
    |> DamageInfo.setWeaponBodyPartGroup (record.verb.verbProps.AdjustedLinkedBodyPartsGroup record.verb.tool)

  member this.CreateRangedDamageInfo record =
    let amount = record.baseDamage * this.amount

    DamageInfo(
      this.def,
      amount,
      this.RangedArmorPen record,
      angle = record.projectile.ExactRotation.eulerAngles.y,
      instigator = record.projectile.Launcher,
      intendedTarget = record.projectile.intendedTarget.Thing,
      weapon = record.sourceDef
    )
