namespace Infusion.OnHitWorkers

open RimWorld
open Verse


type MeleeHitRecord =
  { baseDamage: float32
    source: ThingWithComps
    target: Thing
    verb: Verb }


type RangedHitRecord =
  { baseDamage: float32
    map: Map
    projectile: Bullet
    target: Thing option
    sourceDef: ThingDef }


[<AllowNullLiteral>]
type OnHitWorker =
  inherit Editable

  val mutable amount: float32

  val mutable chance: float32

  val mutable selfCast: bool

  new() =
    { amount = 0.0f
      chance = 1.0f
      selfCast = false }

  abstract MeleeHit: MeleeHitRecord -> unit

  abstract BulletHit: RangedHitRecord -> unit

  default this.MeleeHit _ = ()

  default this.BulletHit _ = ()


module OnHitWorker =
  let checkChance (worker: OnHitWorker) = Rand.Chance worker.chance


  /// Gets the melee effect target's current Map.
  let mapMelee selfCast record =
    if selfCast then
      record.source.MapHeld
    else
      record.target.MapHeld


  /// Gets the ranged effect target's current Map.
  let mapRanged selfCast record =
    if selfCast then
      record.projectile.Launcher.MapHeld
    else
      record.map


  /// Gets the melee effect target's current position.
  let posMelee selfCast record =
    if selfCast then
      record.source.PositionHeld
    else
      record.target.PositionHeld


  /// Gets the ranged effect target's current position.
  let posRanged selfCast record =
    if selfCast then
      record.projectile.Launcher.PositionHeld
    else
      record.projectile.Position


  /// Gets the melee effect target's current (Map, position).
  let mapPosMelee selfCast record =
    (mapMelee selfCast record, posMelee selfCast record)


  /// Gets the ranged effect target's current (Map, position).
  let mapPosRanged selfCast record =
    (mapRanged selfCast record, posRanged selfCast record)
