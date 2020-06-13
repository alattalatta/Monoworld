module Poet.Lyric.Thing

open Verse


let getComp<'a when 'a :> ThingComp and 'a: null> (thing: Thing) = Option.ofObj (thing.TryGetComp<'a>())

let onSameMap (a: Thing) (b: Thing) = a.Map = b.Map

let isDestroyed (t: Thing) = t.Destroyed
