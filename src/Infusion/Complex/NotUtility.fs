namespace Infusion.Complex

open Infusion


type NotUtility() =
    inherit Complex<InfusionDef>()

    override this.Match thing _ =
        if not thing.def.IsApparel then
            false
        else if thing.def.apparel.bodyPartGroups.Count = 0 then
            false
        else
            // if it covers only a single group, it can't be the utility slot.
            not (
                thing.def.apparel.bodyPartGroups.Count = 1
                && (thing.def.apparel.bodyPartGroups.Item 0).defName = "Waist"
            )

    override this.BuildRequirementString() =
        Some ResourceBank.Strings.Complex.notUtility
