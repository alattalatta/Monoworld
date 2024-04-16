namespace Infusion.OnHitWorkers

open RimWorld
open Verse


type SpawnFilth =
  inherit OnHitWorker

  val mutable def: ThingDef

  new() =
    { inherit OnHitWorker()

      def = null }

  override this.BulletHit record =
    if record.projectile.Position.Walkable record.map then
      do
        FilthMaker.TryMakeFilth(record.projectile.Position, record.map, this.def)
        |> ignore

  override this.MeleeHit record =
    if record.target.Position.Walkable record.target.Map then
      do
        FilthMaker.TryMakeFilth(record.target.PositionHeld, record.source.MapHeld, this.def)
        |> ignore
