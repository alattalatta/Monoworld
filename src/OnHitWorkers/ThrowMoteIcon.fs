namespace Infusion.OnHitWorkers

open RimWorld
open Verse


type ThrowMoteIcon =
    inherit OnHitWorker

    val mutable def: ThingDef

    new() =
        { inherit OnHitWorker()

          def = null }

    override this.MeleeHit record =
        let map =
            if this.selfCast then record.source.MapHeld else record.target.Map
            |> Option.ofObj

        let position =
            if this.selfCast then record.verb.Caster.Position else record.target.Position

        this.ThrowMote map position

    override this.BulletHit record =
        let map =
            if this.selfCast then record.projectile.Launcher.Map else record.map
            |> Option.ofObj

        let position =
            if this.selfCast then record.projectile.Launcher.Position else record.projectile.Position

        this.ThrowMote map position

    member private this.ThrowMote (map: Map option) (position: IntVec3) =
        map
        |> Option.filter (position.Fogged >> not)
        |> Option.iter (fun m ->
            do MoteMaker.ThrowMetaIcon(position, m, this.def)
               |> ignore)
