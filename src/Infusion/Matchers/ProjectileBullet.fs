namespace Infusion.Matchers

open RimWorld

open Infusion


type ProjectileBullet() =
    inherit Matcher<InfusionDef>()

    override this.Match thing _ =
        if not thing.def.IsRangedWeapon then
            true
        else
            Seq.tryHead thing.def.Verbs
            |> Option.bind (fun a -> Option.ofObj a.defaultProjectile)
            |> Option.map (fun a -> a.thingClass = typeof<Bullet>)
            |> Option.defaultValue false
