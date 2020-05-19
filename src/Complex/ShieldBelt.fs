namespace Infusion.Complex

open RimWorld

open Infusion

type ShieldBelt() =
    inherit Complex<InfusionDef>()

    override this.Match thing _ = thing.def.thingClass = typeof<ShieldBelt>

    override this.BuildRequirementString() = Some L18N.Complex.shieldBelt
