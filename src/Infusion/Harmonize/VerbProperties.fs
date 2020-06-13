module Infusion.Harmonize.VerbProperties

open System
open System.Reflection.Emit

open HarmonyLib
open Verse


[<HarmonyPatch(typeof<VerbProperties>, "GetHitChanceFactor")>]
module GetHitChanceFactor =
    /// Changes max accuracy to 200%.
    let Transpiler (instructions: seq<CodeInstruction>) =
        let insts = Array.ofSeq instructions

        let targetOpCodePos =
            insts
            |> Array.tryFindIndex (fun code ->
                code.opcode = OpCodes.Ldc_R4
                && Convert.ToSingle code.operand = 1.0f)

        do match targetOpCodePos with
           | Some s -> do insts.[s].operand <- 2.0f
           | None ->
               do Log.Warning
                   ("[Infusion 2] Couldn't find matching opCode for VerbProperties.GetHitChanceFactor(). Accuracy overcapping can't be applied.")

        seq insts
