namespace Infusion.OnHitWorkers

open RimWorld
open Verse

open Infusion


type ThrowFleck =
  inherit OnHitWorker

  val mutable def: FleckDef

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
      |> this.ThrowFleck
    | VerbCastedRecordRanged r when this.onRangedCast ->
      this.MapPosOf(OnHitRecordMeleeCast r)
      |> this.ThrowFleck
    | _ -> ()

  override this.BulletHit record =
    if this.onRangedImpact then
      this.MapPosOf(OnHitRecordRangedImpact record)
      |> this.ThrowFleck

  override this.MeleeHit record =
    if this.onMeleeImpact then
      this.MapPosOf(OnHitRecordMeleeHit record)
      |> this.ThrowFleck

  override this.WearerDowned pawn _ =
    this.ThrowFleck(pawn.Map, pawn.Position)
    true

  member private this.ThrowFleck ((map, pos): (Map * IntVec3)) =
    if not (pos.Fogged map) then
      FleckMaker.ThrowMetaIcon(pos, map, this.def) |> ignore
