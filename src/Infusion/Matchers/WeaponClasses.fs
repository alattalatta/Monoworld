namespace Infusion.Matchers

open RimWorld

open Infusion


type WeaponClasses =
    inherit Matcher<InfusionDef>

    val mutable defs: ResizeArray<WeaponClassDef>

    new() =
        { inherit Matcher<InfusionDef>()
          defs = ResizeArray() }

    override this.Match thing _ =
        this.defs
        |> Seq.exists (thing.def.weaponClasses.Contains)

    override this.BuildRequirementString() =
        this.defs
        |> seq
        |> Seq.map (fun it -> it.label)
        |> String.concat ", "
        |> Some
