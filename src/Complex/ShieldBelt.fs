namespace Infusion.Complex

open RimWorld

open Infusion

type ShieldBelt() =
    inherit Complex<InfusionDef>()

    override this.Match thing _ =
        (thing.def.statBases.GetStatValueFromList(StatDefOf.EnergyShieldEnergyMax, 0.0f)) > 0.0f

    override this.BuildRequirementString() = Some L18N.Complex.shieldBelt
