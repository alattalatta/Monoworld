namespace Infusion.OnHitWorkers

open RimWorld
open Verse

open Infusion


type SpawnFilth =
  inherit OnHitWorker

  val mutable def: ThingDef

  new() =
    { inherit OnHitWorker()

      def = null }

  override this.BulletHit record =
    if record.projectile.Position.Walkable record.map then
      FilthMaker.TryMakeFilth(record.projectile.Position, record.map, this.def)
      |> ignore

  override this.MeleeHit record =
    if record.target.Position.Walkable record.target.Map then
      FilthMaker.TryMakeFilth(record.target.PositionHeld, record.source.MapHeld, this.def)
      |> ignore
