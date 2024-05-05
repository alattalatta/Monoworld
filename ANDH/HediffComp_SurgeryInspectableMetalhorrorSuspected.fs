namespace ANDH

open RimWorld
open Verse


[<AllowNullLiteral>]
type HediffComp_SurgeryInspectableMetalhorrorSuspected() =
  inherit HediffComp_SurgeryInspectable()

  override this.DoSurgicalInspection (pawn: Pawn) =
    SurgicalInspectionOutcome.Nothing
