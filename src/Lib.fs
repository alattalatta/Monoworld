module Infusion.Lib

open System
open System.Collections.Generic

let (<&>) f g = (fun x -> f x && g x)

let feq (a: float32) (b: float32) = (abs (a - b)) < 0.01f

let feq0 = feq 0.0f

let fneq a b = not (feq a b)

let fneq0 = fneq 0.0f

let dictseq (dict: Dictionary<_, _>) =
    seq {
        for entry in dict do
            yield entry
    }

let flip f a b = f b a

let shuffle seq =
    let array = Seq.toArray seq
    let random = Random()
    for i in 0 .. array.Length - 1 do
        let j = random.Next(i, array.Length)
        let pom = array.[i]
        array.[i] <- array.[j]
        array.[j] <- pom

    array |> Array.toSeq

let tap fn a =
    do fn a
    a

module Option =
    let tap f a =
        match a with
        | Some a2 ->
            do f (a2)
            a
        | None -> None

    let tapNone f a =
        match a with
        | Some a2 -> a
        | None ->
            do f ()
            None

    let ofSeq a =
        if Seq.length a = 0 then None else Some a
