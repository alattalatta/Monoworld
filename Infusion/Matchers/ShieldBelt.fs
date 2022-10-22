namespace Infusion.Matchers

open RimWorld

open Infusion


type ShieldBelt() =
    inherit Matcher<InfusionDef>()

    override this.Match thing _ =
        (thing.def.statBases.GetStatValueFromList(StatDefOf.EnergyShieldEnergyMax, 0.0f)) > 0.0f

    override this.BuildRequirementString() =
        Some ResourceBank.Strings.Matchers.shieldBelt
