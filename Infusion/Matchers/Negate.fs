namespace Infusion.Matchers

open Infusion


type Negate =
  inherit Matcher<InfusionDef>

  // assumes that this isn't null
  val mutable value: Matcher<InfusionDef>

  new() =
    { inherit Matcher<InfusionDef>()
      value = null }

  override this.Match thing infDef = not (this.value.Match thing infDef)

  override this.BuildRequirementString() =
    this.value.BuildRequirementString()
    |> Option.map (ResourceBank.Strings.Matchers.negate >> string)

  override this.ConfigErrors() =
    if isNull this.value then
      seq { "no value" }
    else
      Seq.empty
