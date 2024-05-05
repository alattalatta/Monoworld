module ANDH.MetalHorrorUtility

open HarmonyLib
open RimWorld
open Verse


[<HarmonyPatch(typeof<MetalhorrorUtility>, "Infect")>]
module Infect =
  let Postfix (pawn: Pawn, source: Pawn, descKey: string, descResolved: string) =
    Option.ofObj (pawn.health.hediffSet.GetFirstHediffOfDef ResourceBank.Defs.ANDH_MetalhorrorImplant_FalseAlarm)
    |> Option.iter (pawn.health.RemoveHediff)
