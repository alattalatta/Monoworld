namespace Infusion.Matchers

open Infusion


type NotUtility() =
  inherit Matcher<InfusionDef>()

  override this.BuildRequirementString() =
    Some ResourceBank.Strings.Matchers.notUtility

  override this.Match thing _ =
    if not thing.def.IsApparel then
      false
    else if thing.def.apparel.bodyPartGroups.Count = 0 then
      false
    else
      // if it covers only a single group, it can't be an utility slot.
      not (
        thing.def.apparel.bodyPartGroups.Count = 1
        && (thing.def.apparel.bodyPartGroups.Item 0).defName = "Waist"
      )
