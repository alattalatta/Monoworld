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
        Option.ofObj thing.def.weaponClasses
        |> Option.map (fun weaponClasses -> this.defs |> Seq.exists (weaponClasses.Contains))
        |> Option.defaultValue false

    override this.BuildRequirementString() =
        this.defs
        |> seq
        |> Seq.map (fun it -> it.label)
        |> String.concat ", "
        |> Some
