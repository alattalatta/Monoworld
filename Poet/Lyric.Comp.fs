module Poet.Lyric.Comp

open Verse


let ofHediff<'a when 'a :> HediffComp and 'a: null> (hediff: Hediff) = Option.ofObj (hediff.TryGetComp<'a>())

let ofThing<'a when 'a :> ThingComp and 'a: null> (thing: Thing) = Option.ofObj (thing.TryGetComp<'a>())

let getParent<'a when 'a :> ThingComp and 'a: null> (comp: 'a) = comp.parent
