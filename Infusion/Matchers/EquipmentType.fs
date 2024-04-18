namespace Infusion.Matchers

open Infusion


type EquipmentType =
  inherit Matcher<InfusionDef>

  val mutable apparel: bool
  val mutable melee: bool
  val mutable ranged: bool

  new() =
    { inherit Matcher<InfusionDef>()
      apparel = false
      melee = false
      ranged = false }

  override this.BuildRequirementString() =
    let apparel =
      if this.apparel then
        Some ResourceBank.Strings.Matchers.apparel
      else
        None

    let melee =
      if this.melee then
        Some ResourceBank.Strings.Matchers.melee
      else
        None

    let ranged =
      if this.ranged then
        Some ResourceBank.Strings.Matchers.ranged
      else
        None

    [ apparel; melee; ranged ]
    |> List.choose id
    |> String.concat ", "
    |> Some

  override this.Match thing _ =
    if thing.def.IsApparel then
      this.apparel
    elif thing.def.IsMeleeWeapon then
      this.melee
    elif thing.def.IsRangedWeapon then
      this.ranged
    else
      false
