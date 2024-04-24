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

  member this.MeleeArmorPen(verb: Verb) =
    if this.armorPenetration < 0.0f then
      verb.verbProps.AdjustedArmorPenetration(verb, verb.CasterPawn)
    else
      this.armorPenetration

  member this.RangedArmorPen(projectile: Projectile) =
    if this.armorPenetration < 0.0f then
      projectile.ArmorPenetration
    else
      this.armorPenetration
