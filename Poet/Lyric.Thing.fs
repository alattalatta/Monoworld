module Poet.Lyric.Thing

open Verse


let getComp<'a when 'a :> ThingComp and 'a: null> (thing: Thing) = Option.ofObj (thing.TryGetComp<'a>())

let isDestroyed (t: Thing) = t.Destroyed

let onSameMap (a: Thing) (b: Thing) = a.Map = b.Map

let placeThingNear (coord: IntVec3) (map: Map) (thing: Thing) =
  if GenPlace.TryPlaceThing(thing, coord, map, ThingPlaceMode.Near) then
    Some thing
  else
    None
