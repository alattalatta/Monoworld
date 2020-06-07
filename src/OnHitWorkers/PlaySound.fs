namespace Infusion.OnHitWorkers

open RimWorld
open UnityEngine
open Verse
open Verse.Sound


type PlaySound =
    inherit OnHitWorker

    val mutable def: SoundDef

    new() = { def = null }

    override this.MeleeHit record =
        let (map, pos) =
            OnHitWorker.mapPosMelee this.selfCast record

        let ti = TargetInfo(pos, map)

        this.def.PlayOneShot(SoundInfo.InMap(ti))

    override this.BulletHit record =
        let (map, pos) =
            OnHitWorker.mapPosRanged this.selfCast record

        let ti = TargetInfo(pos, map)

        this.def.PlayOneShot(SoundInfo.InMap(ti))
