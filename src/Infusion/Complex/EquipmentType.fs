namespace Infusion.Complex

open Infusion


type EquipmentType =
    inherit Complex<InfusionDef>

    val mutable apparel: bool
    val mutable melee: bool
    val mutable ranged: bool

    new() =
        { inherit Complex<InfusionDef>()
          apparel = false
          melee = false
          ranged = false }

    override this.Match thing _ =
        if thing.def.IsApparel then this.apparel
        elif thing.def.IsMeleeWeapon then this.melee
        elif thing.def.IsRangedWeapon then this.ranged
        else false

    override this.BuildRequirementString() =
        let apparel =
            if this.apparel then Some L10N.Complex.apparel else None

        let melee =
            if this.melee then Some L10N.Complex.melee else None

        let ranged =
            if this.ranged then Some L10N.Complex.ranged else None

        [ apparel; melee; ranged ]
        |> List.choose id
        |> String.concat ", "
        |> Some
