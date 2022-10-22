namespace Infusion.Matchers

open Infusion


type Negate =
    inherit Matcher<InfusionDef>

    val mutable value: Matcher<InfusionDef>

    new() =
        { inherit Matcher<InfusionDef>()
          value = null }

    member this.Value = Option.ofObj this.value

    override this.Match thing infDef =
        this.Value
        |> Option.map (fun value -> value.Match thing infDef)
        |> Option.map not
        |> Option.defaultValue true

    override this.BuildRequirementString() =
        this.Value
        |> Option.bind (fun value -> value.BuildRequirementString())
        |> Option.map (ResourceBank.Strings.Matchers.negate >> string)
