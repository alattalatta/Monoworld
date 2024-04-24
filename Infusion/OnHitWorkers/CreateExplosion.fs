namespace Infusion.OnHitWorkers

open System
open Verse

open Infusion


// [todo] Associate with battle logs
type CreateExplosion =
  inherit DamageBase

  val mutable postExplosionGasType: Nullable<GasType>
  val mutable radius: float32
  val mutable onMeleeCast: bool
  val mutable onMeleeImpact: bool

  new() =
    { inherit DamageBase()

      postExplosionGasType = Nullable()
      radius = 0.5f

      onMeleeCast = true
      onMeleeImpact = true }

  override this.AfterAttack record =
    match record with
    | VerbCastedRecordMelee mr when this.onMeleeCast ->
      let (map, pos) = this.MapPosOf(OnHitRecordMeleeCast mr)

      GenExplosion.DoExplosion(
        pos,
        map,
        this.radius,
        this.def,
        armorPenetration = this.MeleeArmorPen mr.verb,
        damAmount = int (mr.baseDamage * this.amount),
        instigator = mr.source,
        intendedTarget = mr.target,
        postExplosionGasType = this.postExplosionGasType,
        weapon = mr.source.def
      )
    | _ -> ()

  override this.BulletHit record =
    let (map, pos) = this.MapPosOf(OnHitRecordRangedImpact record)

    GenExplosion.DoExplosion(
      pos,
      map,
      this.radius,
      this.def,
      armorPenetration = this.RangedArmorPen record.projectile,
      damAmount = int (record.baseDamage * this.amount),
      instigator = record.projectile.Launcher,
      intendedTarget = record.projectile.intendedTarget.Thing,
      postExplosionGasType = this.postExplosionGasType,
      projectile = record.projectile.def,
      weapon = record.source.def
    )

  override this.MeleeHit record =
    if this.onMeleeImpact then
      let (map, pos) = this.MapPosOf(OnHitRecordMeleeHit record)

      GenExplosion.DoExplosion(
        pos,
        map,
        this.radius,
        this.def,
        armorPenetration = this.MeleeArmorPen record.verb,
        damAmount = int (record.baseDamage * this.amount),
        instigator = record.source,
        intendedTarget = record.target,
        postExplosionGasType = this.postExplosionGasType,
        weapon = record.source.def
      )
