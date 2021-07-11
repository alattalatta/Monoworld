namespace Infusion.Matchers

open RimWorld

open Infusion


type WeaponClasses =
    inherit Matcher<InfusionDef>

    val mutable classes: ResizeArray<WeaponClassDef>

    new() =
        { inherit Matcher<InfusionDef>()
          classes = ResizeArray() }

    override this.Match thing _ =
        this.classes
        |> Seq.exists (thing.def.weaponClasses.Contains)

    override this.BuildRequirementString() =
        this.classes
        |> seq
        |> Seq.map (fun it -> it.label)
        |> String.concat ", "
        |> Some
