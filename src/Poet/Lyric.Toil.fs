module Poet.Lyric.Toil

open Verse.AI

let setInitAction fn (toil: Toil) =
    do toil.initAction <- new System.Action(fn)
    toil

let setTickAction fn (toil: Toil) =
    do toil.tickAction <- new System.Action(fn)
    toil

let addFailOn fn (toil: Toil) =
    do toil.FailOn(new System.Func<bool>(fn)) |> ignore
    toil

let addFailOnCannotTouch targetIndex pathEndMode (toil: Toil) =
    do toil.FailOnCannotTouch(targetIndex, pathEndMode)
       |> ignore
    toil

let addFailOnSomeoneInteracting targetIndex (toil: Toil) =
    do toil.FailOnSomeonePhysicallyInteracting(targetIndex)
       |> ignore
    toil

let setDefaultCompleteMode mode (toil: Toil) =
    do toil.defaultCompleteMode <- mode
    toil

let addProgressBar targetIndex (toil: Toil) =
    do toil.WithProgressBarToilDelay(targetIndex)
       |> ignore
    toil
