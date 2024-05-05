namespace ANDH

open Verse


[<AllowNullLiteral>]
type HediffCompProperties_SurgeryInspectableMetalHorrorSuspected =
  inherit HediffCompProperties_SurgeryInspectable

  new() as this =
    { inherit HediffCompProperties_SurgeryInspectable() } then
      this.compClass <- typeof<HediffComp_SurgeryInspectableMetalhorrorSuspected>
