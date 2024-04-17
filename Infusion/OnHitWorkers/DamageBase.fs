namespace Infusion.OnHitWorkers

open Verse

open Infusion


type DamageBase =
  inherit OnHitWorker

  val mutable armorPenetration: float32
  val mutable def: DamageDef

  new() =
    { inherit OnHitWorker()

      armorPenetration = -1.0f
      def = null }

  member this.MeleeArmorPen(record: MeleeHitRecord) =
    if this.armorPenetration < 0.0f then
      record.verb.verbProps.AdjustedArmorPenetration(record.verb, record.verb.CasterPawn)
    else
      this.armorPenetration

  member this.RangedArmorPen(record: RangedHitRecord) =
    if this.armorPenetration < 0.0f then
      record.projectile.ArmorPenetration
    else
      this.armorPenetration
