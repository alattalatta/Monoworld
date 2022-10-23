module Infusion.DefFields

open System

open RimWorld
open Verse


/// Quality -> Value map.
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


/// Retrieves a value for the quality from a QualityMap.
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


[<AllowNullLiteral>]
type Migration<'a when 'a :> Def and 'a: null> =
  val mutable remove: bool
  val mutable replace: 'a

  new() = { remove = false; replace = null }

  member this.Replace = Option.ofObj this.replace
