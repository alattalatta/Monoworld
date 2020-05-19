namespace Infusion.Complex

open Infusion

type Negate =
    inherit Complex<InfusionDef>

    val mutable value: Complex<InfusionDef>

    new() =
        { inherit Complex<InfusionDef>()
          value = null }

    member this.Value = Option.ofObj this.value

    override this.Match thing infDef =
        this.Value
        |> Option.map (fun value -> value.Match thing infDef)
        |> Option.defaultValue true

    override this.BuildRequirementString() =
        this.Value
        |> Option.bind (fun value -> value.BuildRequirementString())
        |> Option.map (L18N.Complex.negate >> string)
