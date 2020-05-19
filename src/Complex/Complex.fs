namespace Infusion.Complex

open RimWorld
open Verse

[<AllowNullLiteral>]
type Complex<'a when 'a :> Def>() =

    abstract Match: ThingWithComps -> 'a -> bool

    abstract BuildRequirementString: unit -> string option

    default this.Match _ _ = true

    default this.BuildRequirementString() = None
