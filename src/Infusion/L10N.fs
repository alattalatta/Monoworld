module Infusion.L10N

open Poet.Lyric.Translation


module Complex =
    let apparel = translate "Infusion.Complex.Apparel"
    let melee = translate "Infusion.Complex.Melee"
    let ranged = translate "Infusion.Complex.Ranged"

    let negate str = translate1 "Infusion.Complex.Negate" str

    let shieldBelt = translate "Infusion.Complex.ShieldBelt"
