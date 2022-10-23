namespace Infusion.OnHitWorkers

open Verse


// [todo] Associate with battle logs
type CreateExplosion =
  inherit DamageBase

  val mutable radius: float32

  new() =
    { inherit DamageBase()

      radius = 0.5f }

  override this.MeleeHit record =
    let (map, pos) = OnHitWorker.mapPosMelee this.selfCast record

    do
      GenExplosion.DoExplosion(
        pos,
        map,
        this.radius,
        this.def,
        armorPenetration = this.MeleeArmorPen record,
        damAmount = int (record.baseDamage * this.amount),
        instigator = record.source,
        intendedTarget = record.target,
        weapon = record.source.def
      )

  override this.BulletHit record =
    let (map, pos) = OnHitWorker.mapPosRanged this.selfCast record

    do
      GenExplosion.DoExplosion(
        pos,
        map,
        this.radius,
        this.def,
        armorPenetration = this.RangedArmorPen record,
        damAmount = int (record.baseDamage * this.amount),
        instigator = record.projectile.Launcher,
        intendedTarget = record.projectile.intendedTarget.Thing,
        projectile = record.projectile.def,
        weapon = record.sourceDef
      )
