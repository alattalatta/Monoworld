namespace Infusion.Matchers

open Verse


[<AllowNullLiteral>]
type Matcher<'a when 'a :> Def> =
    val mutable requirementString: string

    new() = { requirementString = "" }

    member this.RequirementString =
        if this.requirementString.NullOrEmpty() then
            this.BuildRequirementString()
        else
            Some this.requirementString

    abstract Match : ThingWithComps -> 'a -> bool

    abstract BuildRequirementString : unit -> string option

    default this.Match _ _ = true

    default this.BuildRequirementString() = None
