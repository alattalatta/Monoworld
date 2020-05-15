module Infusion.StatMod

open System.Text

open RimWorld
open Verse

open Lib


type StatMod =
    { offset: float32
      multiplier: float32 }

    override this.ToString() =
        let sb = StringBuilder()

        if fneq0 this.multiplier then
            do sb.Append(this.multiplier.ToString("+0.##%;-0.##%"))
               |> ignore

            if fneq0 this.multiplier && fneq0 this.offset
            then do sb.Append(", ") |> ignore

        if fneq0 this.offset then
            do sb.Append(this.offset.ToString("+0.##;-0.##"))
               |> ignore

        string sb

    static member (+)(a: StatMod, b: StatMod) =
        { offset = a.offset + b.offset
          multiplier = a.multiplier + b.multiplier }

    static member empty = { offset = 0.0f; multiplier = 0.0f }

let applyTo (value: float32) (statMod: StatMod) =
    value
    + statMod.offset
    + value * statMod.multiplier

let stringForStat (stat: StatDef) (statMod: StatMod) =
    let sb = StringBuilder()

    // multiplier
    if fneq0 statMod.multiplier then
        do sb.Append(statMod.multiplier.ToString("+0.##%;-0.##%"))
           |> ignore

        // separator
        if fneq0 statMod.offset then do sb.Append(", ") |> ignore

    // offset
    if fneq0 statMod.offset then
        let styled =
            statMod.offset.ToStringByStyle(stat.toStringStyle)
            |> (if isNull stat.formatString
                then id
                else (fun str -> System.String.Format(stat.formatString, str)))

        // force add plus sign, StatDef#formatString don't have it
        if statMod.offset > 0.0f then do sb.Append("+") |> ignore

        do sb.Append(styled) |> ignore

    string sb
