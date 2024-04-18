module Infusion.Harmonize.Pawn_EquipmentTracker

open HarmonyLib
open Poet.Lyric
open Verse

open Infusion


[<HarmonyPatch(typeof<Pawn_EquipmentTracker>, "GetGizmos")>]
module GetGizmos =
  let Postfix (returned: Gizmo seq, __instance: Pawn_EquipmentTracker) =
    let pawn = __instance.pawn

    if pawn.IsColonistPlayerControlled
       || pawn.IsColonyMech
       || pawn.IsColonyMutantPlayerControlled then
      Seq.tryHead __instance.AllEquipmentListForReading
      |> Option.bind Thing.getComp<CompInfusion>
      // only adds the effect gizmo to prevent unnecessary compat complications
      |> Option.bind (fun comp -> comp.EffectGizmo)
      |> Option.map (fun gizmo ->
        seq {
          yield! returned
          yield gizmo :> Gizmo
        })
      |> Option.defaultValue returned
    else
      returned
