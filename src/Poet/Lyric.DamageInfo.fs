module Poet.Lyric.DamageInfo

open Verse


let setAngle angle (di: DamageInfo) =
  di.SetAngle angle
  di

let setBodyRegion height depth (di: DamageInfo) =
  di.SetBodyRegion(height, depth)
  di

let setWeaponBodyPartGroup bodyPartGroup (di: DamageInfo) =
  di.SetWeaponBodyPartGroup(bodyPartGroup)
  di
