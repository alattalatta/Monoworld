namespace Infusion.OnHitWorkers

open Verse
open Verse.Sound

open Infusion


type PlaySound =
  inherit OnHitWorker

  val mutable def: SoundDef

  new() = { def = null }

  override this.BulletHit record =
    let (map, pos) = OnHitWorker.mapPosRanged this.selfCast record

    let ti = TargetInfo(pos, map)

    this.def.PlayOneShot(SoundInfo.InMap(ti))

  override this.MeleeHit record =
    let (map, pos) = OnHitWorker.mapPosMelee this.selfCast record

    let ti = TargetInfo(pos, map)

    this.def.PlayOneShot(SoundInfo.InMap(ti))
    
  override this.WearerDowned pawn _ =
    let ti = TargetInfo(pawn.Position, pawn.Map)

    this.def.PlayOneShot(SoundInfo.InMap(ti))

    true