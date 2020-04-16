module Infusion.StatMod

open System.Text

open RimWorld
open Verse

open Lib

type StatMod =
    val mutable offset: float32
    val mutable multiplier: float32

    new() =
        { offset = 0.f
          multiplier = 1.f }

    new(ofst, mult) =
        { offset = ofst
          multiplier = mult }

    member this.ToStringForStat(stat: StatDef) =
        let sb = StringBuilder()

        if fneq this.multiplier 1.0f then do sb.Append("x").Append(this.multiplier.ToString("0.##%")) |> ignore
        if fneq this.multiplier 1.0f && fneq0 this.offset then do sb.Append(", ") |> ignore
        if fneq0 this.offset then
            let styled =
                this.offset.ToStringByStyle(stat.toStringStyle)
                |> (if isNull stat.formatString
                    then id
                    else (fun str -> System.String.Format(stat.formatString, str)))
            do sb.Append(styled) |> ignore

        string sb

    override this.Equals(ob: obj) =
        match ob with
        | :? StatMod as statMod -> feq this.offset statMod.offset && feq this.multiplier statMod.multiplier
        | _ -> false

    override this.GetHashCode() = this.offset.GetHashCode() ^^^ this.multiplier.GetHashCode()

    override this.ToString() =
        let sb = StringBuilder()

        if fneq this.multiplier 1.0f then do sb.Append("x").Append(this.multiplier.ToString("0.##%")) |> ignore
        if fneq this.multiplier 1.0f && fneq0 this.offset then do sb.Append(", ") |> ignore
        if fneq0 this.offset then do sb.Append(this.offset.ToString("+0.##;-0.##")) |> ignore

        string sb

    static member empty = StatMod()

    static member (+) (a: StatMod, b: StatMod) = StatMod(a.offset + b.offset, a.multiplier * b.multiplier)

let applyTo (value: float32) (statMod: StatMod) = value * statMod.multiplier + statMod.offset
