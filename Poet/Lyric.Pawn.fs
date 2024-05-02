module Poet.Lyric.Pawn

open RimWorld
open Verse


let isDead (pawn: Pawn) = pawn.Dead

let getApparels (pawn: Pawn) : option<list<Apparel>> =
  Option.ofObj pawn.apparel
  |> Option.map (fun tracker -> List.ofSeq tracker.WornApparel)

let getEquipments (pawn: Pawn) : option<list<ThingWithComps>> =
  Option.ofObj pawn.equipment
  |> Option.map (fun tracker -> List.ofSeq tracker.AllEquipmentListForReading)

let getPrimaryEquipment (pawn: Pawn): ThingWithComps option =
  Option.ofObj pawn.equipment
  |> Option.bind (fun eq -> Option.ofObj eq.Primary)