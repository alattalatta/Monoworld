namespace Infusion.Matchers

open Verse

open Infusion


type HasVerb_LaunchProjectile() =
  inherit Matcher<InfusionDef>()

  override this.Match thing _ =
    if not thing.def.IsRangedWeapon then
      true
    else
      Seq.tryHead thing.def.Verbs
      |> Option.filter (fun a -> a.verbClass.IsSubclassOf(typeof<Verb_LaunchProjectile>))
      |> Option.isSome
