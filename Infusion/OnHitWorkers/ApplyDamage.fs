namespace Infusion.OnHitWorkers

open Poet.Lyric
open Verse

open Infusion
open VerseTools


// [todo] Associate with battle logs
type ApplyDamage =
  inherit DamageBase

  val mutable onMeleeCast: bool
  val mutable onMeleeImpact: bool

  new() =
    { inherit DamageBase()

      onMeleeCast = true
      onMeleeImpact = true }

  override this.AfterAttack record =
    match record with
    | VerbCastedRecordMelee mr when this.onMeleeCast ->
      if Pawn.isAliveAndWell mr.target then
        this.CreateMeleeDamageInfo mr
        |> mr.target.TakeDamage
        |> ignore
    | _ -> ()

  override this.BulletHit record =
    do
      record.target
      |> Option.filter Pawn.isAliveAndWell
      |> Option.iter (fun t ->
        this.CreateRangedDamageInfo record
        |> t.TakeDamage
        |> ignore)

  override this.MeleeHit record =
    if Pawn.isAliveAndWell record.target then
      this.CreateMeleeDamageInfo record
      |> record.target.TakeDamage
      |> ignore

  member private this.CreateMeleeDamageInfo record =
    let amount = record.baseDamage * this.amount

    let direction =
      (record.target.Position
       - record.verb.caster.Position)
        .ToVector3()

    DamageInfo(
      this.def,
      Rand.Range(amount * 0.8f, amount * 1.2f),
      this.MeleeArmorPen record.verb,
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
      this.RangedArmorPen record.projectile,
      angle = record.projectile.ExactRotation.eulerAngles.y,
      instigator = record.projectile.Launcher,
      intendedTarget = record.projectile.intendedTarget.Thing,
      weapon = record.source.def
    )
