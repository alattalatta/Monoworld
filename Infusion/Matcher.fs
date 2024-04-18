namespace Infusion.Matchers

open Verse


/// Filter for infusion applicability.
[<AllowNullLiteral>]
// For now, 'a = InfusionDef
type Matcher<'a when 'a :> Def> =
  inherit Editable

  val mutable requirementString: string

  new() = { requirementString = "" }

  member this.RequirementString =
    if this.requirementString.NullOrEmpty() then
      this.BuildRequirementString()
    else
      Some this.requirementString

  abstract BuildRequirementString: unit -> string option

  abstract Match: ThingWithComps -> 'a -> bool

  default this.BuildRequirementString() = None

  default this.Match _ _ = true
