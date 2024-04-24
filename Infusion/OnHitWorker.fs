namespace Infusion

open RimWorld
open Verse


type private VerbRecordData =
  {| baseDamage: float32
     source: ThingWithComps
     target: Thing
     verb: Verb |}

/// Records for verb finished. Used in AfterAttacks.
type VerbCastedRecord =
  | VerbCastedRecordMelee of VerbRecordData
  | VerbCastedRecordRanged of VerbRecordData


type ProjectileRecord =
  { baseDamage: float32
    map: Map
    projectile: Bullet
    source: ThingWithComps
    target: Thing option }

/// Records for every OnHitWorkers. Used in utilities.
type OnHitRecord =
  | OnHitRecordMeleeCast of VerbRecordData
  | OnHitRecordMeleeHit of VerbRecordData
  | OnHitRecordRangedCast of VerbRecordData
  | OnHitRecordRangedImpact of ProjectileRecord


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

  abstract Chance: float32

  abstract AfterAttack: VerbCastedRecord -> unit

  abstract BulletHit: ProjectileRecord -> unit

  abstract MeleeHit: VerbRecordData -> unit

  abstract WearerDowned: Pawn -> Apparel -> bool

  default this.Chance = this.chance

  default this.AfterAttack _ = ()

  default this.BulletHit _ = ()

  default this.MeleeHit _ = ()

  default this.WearerDowned _ _ = true

  member this.MapOf (record: OnHitRecord) =
    if this.selfCast then
      match record with
      | OnHitRecordMeleeCast r | OnHitRecordMeleeHit r | OnHitRecordRangedCast r -> r.source.MapHeld
      | OnHitRecordRangedImpact r -> r.source.MapHeld
    else
      match record with
      | OnHitRecordMeleeCast r | OnHitRecordMeleeHit r | OnHitRecordRangedCast r -> r.target.MapHeld
      | OnHitRecordRangedImpact r -> r.map

  member this.PosOf (record: OnHitRecord) =
    if this.selfCast then
      match record with
      | OnHitRecordMeleeCast r | OnHitRecordMeleeHit r | OnHitRecordRangedCast r -> r.source.PositionHeld
      | OnHitRecordRangedImpact r -> r.source.PositionHeld
    else
      match record with
      | OnHitRecordMeleeCast r | OnHitRecordMeleeHit r | OnHitRecordRangedCast r -> r.target.PositionHeld
      | OnHitRecordRangedImpact r -> r.projectile.Position

  member this.MapPosOf (record: OnHitRecord) =
    (this.MapOf record, this.PosOf record)


module OnHitWorker =
  let checkChance (worker: OnHitWorker) = Rand.Chance worker.Chance
