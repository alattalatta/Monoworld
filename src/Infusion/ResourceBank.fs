module Infusion.ResourceBank

open Poet.Lyric.Translation
open UnityEngine
open Verse


[<StaticConstructorOnStartup>]
type Textures() =
    static let flame =
        ContentFinder<Texture2D>.Get("Things/Special/Fire/FireA")

    static member Flame = flame


module Strings =
    module Gizmo =
        let label = translate "Infusion.EffectGizmo"

        let desc =
            translate "Infusion.EffectGizmo.Description"
