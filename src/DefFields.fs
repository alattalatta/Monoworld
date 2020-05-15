module Infusion.DefFields

open System

open RimWorld
open Verse

// [todo] group with modules

type Allowance =
    val mutable apparel: bool
    val mutable melee: bool
    val mutable ranged: bool

    new() =
        { apparel = false
          melee = false
          ranged = false }

type ExtraExplosion =
    val mutable amount: int
    val mutable chance: float32
    val mutable def: DamageDef
    val mutable radius: float32

    new() =
        { amount = -1
          chance = 1.0f
          def = DamageDefOf.Bomb
          radius = 1.4f }

type QualityMap =
    val mutable awful: float32
    val mutable poor: float32
    val mutable normal: float32
    val mutable good: float32
    val mutable excellent: float32
    val mutable masterwork: float32
    val mutable legendary: float32

    new() =
        { awful = 0.0f
          poor = 0.0f
          normal = 0.0f
          good = 0.0f
          excellent = 0.0f
          masterwork = 0.0f
          legendary = 0.0f }

let valueFor quality (qmap: QualityMap) =
    match quality with
    | QualityCategory.Awful -> qmap.awful
    | QualityCategory.Poor -> qmap.poor
    | QualityCategory.Normal -> qmap.normal
    | QualityCategory.Good -> qmap.good
    | QualityCategory.Excellent -> qmap.excellent
    | QualityCategory.Masterwork -> qmap.masterwork
    | QualityCategory.Legendary -> qmap.legendary
    | _ -> raise (ArgumentException(sprintf "Unknown quality received: %A" quality))


type Position =
    | Prefix = 0
    | Suffix = 1


type DamageType =
    | Anything = 0
    | Blunt = 1
    | Sharp = 2


type Requirements =
    val mutable allowance: Allowance
    val mutable techLevel: ResizeArray<TechLevel>
    val mutable needBulletClass: bool
    val mutable meleeDamageType: DamageType

    new() =
        { allowance = Allowance()
          techLevel = ResizeArray()
          needBulletClass = false
          meleeDamageType = DamageType.Anything }


type Tier =
    | Awful = 0
    | Poor = 1
    | Common = 2
    | Uncommon = 3
    | Rare = 4
    | Epic = 5
    | Legendary = 6
    | Artifact = 7
