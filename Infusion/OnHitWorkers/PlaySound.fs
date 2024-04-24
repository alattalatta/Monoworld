namespace Infusion.OnHitWorkers

open Verse
open Verse.Sound

open Infusion


type PlaySound =
  inherit OnHitWorker

  val mutable def: SoundDef

  val mutable onMeleeCast: bool
  val mutable onMeleeImpact: bool
  val mutable onRangedCast: bool
  val mutable onRangedImpact: bool

  new() =
    { inherit OnHitWorker()

      def = null

      onMeleeCast = true
      onMeleeImpact = true
      onRangedCast = true
      onRangedImpact = true }

  override this.AfterAttack record =
    match record with
    | VerbCastedRecordMelee r when this.onMeleeCast ->
      this.MapPosOf(OnHitRecordMeleeCast r)
      |> this.PlaySound
    | VerbCastedRecordRanged r when this.onRangedCast ->
      this.MapPosOf(OnHitRecordMeleeCast r)
      |> this.PlaySound
    | _ -> ()

  override this.BulletHit record =
    if this.onRangedImpact then
      this.MapPosOf(OnHitRecordRangedImpact record)
      |> this.PlaySound

  override this.MeleeHit record =
    if this.onMeleeImpact then
      this.MapPosOf(OnHitRecordMeleeHit record)
      |> this.PlaySound

  override this.WearerDowned pawn _ =
    this.PlaySound(pawn.Map, pawn.Position)
    true

  member private this.PlaySound((map, pos): (Map * IntVec3)) =
    TargetInfo(pos, map)
    |> SoundInfo.InMap
    |> this.def.PlayOneShot
