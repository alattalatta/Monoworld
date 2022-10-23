namespace Infusion.OnHitWorkers

open RimWorld
open Verse


type ThrowFleck =
  inherit OnHitWorker

  val mutable def: FleckDef

  new() =
    { inherit OnHitWorker()

      def = null }

  override this.MeleeHit record =
    let map =
      (if this.selfCast then
         record.source.MapHeld
       else
         record.target.Map)
      |> Option.ofObj

    let position =
      if this.selfCast then
        record.verb.Caster.Position
      else
        record.target.Position

    this.ThrowFleck map position

  override this.BulletHit record =
    let map =
      (if this.selfCast then
         record.projectile.Launcher.Map
       else
         record.map)
      |> Option.ofObj

    let position =
      if this.selfCast then
        record.projectile.Launcher.Position
      else
        record.projectile.Position

    this.ThrowFleck map position

  member private this.ThrowFleck (map: Map option) (position: IntVec3) =
    map
    |> Option.filter (position.Fogged >> not)
    |> Option.iter (fun m ->
      do
        FleckMaker.ThrowMetaIcon(position, m, this.def)
        |> ignore)
