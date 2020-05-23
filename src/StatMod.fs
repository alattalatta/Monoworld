module Infusion.StatMod

open System.Text

open RimWorld
open Verse

open Lib

type StatMod =
    val mutable offset: float32
    val mutable multiplier: float32

    new() = { offset = 0.0f; multiplier = 0.0f }

    new(ofst, mult) = { offset = ofst; multiplier = mult }

    override this.Equals(ob: obj) =
        match ob with
        | :? StatMod as statMod ->
            feq this.offset statMod.offset
            && feq this.multiplier statMod.multiplier
        | _ -> false

    override this.GetHashCode() =
        this.offset.GetHashCode()
        ^^^ this.multiplier.GetHashCode()

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

    static member empty = StatMod()

    static member (+)(a: StatMod, b: StatMod) = StatMod(a.offset + b.offset, a.multiplier + b.multiplier)

let applyTo (value: float32) (statMod: StatMod) =
    value
    + statMod.offset
    + value * statMod.multiplier

let stringForStat (stat: StatDef) (statMod: StatMod) =
    let sb = StringBuilder()

    // multiplier
    if fneq0 statMod.multiplier then
        do sb.Append(statMod.multiplier.ToString("x+0.##%;x-0.##%"))
           |> ignore

        // separator
        if fneq0 statMod.offset then do sb.Append(", ") |> ignore

    // offset
    if fneq0 statMod.offset then
        let styled =
            statMod.offset.ToStringByStyle(stat.ToStringStyleUnfinalized)
            |> (if isNull stat.formatString
                then id
                else (fun str -> System.String.Format(stat.formatString, str)))

        // force add plus sign, StatDef#formatString don't have it
        if statMod.offset > 0.0f then do sb.Append("+") |> ignore

        do sb.Append(styled) |> ignore

    string sb
