module Poet.Lyric.Toil

open Verse.AI

let setInitAction fn (toil: Toil) =
  do toil.initAction <- new System.Action(fn)
  toil

let setTickAction fn (toil: Toil) =
  do toil.tickAction <- new System.Action(fn)
  toil

let setDefaultCompleteMode mode (toil: Toil) =
  do toil.defaultCompleteMode <- mode
  toil

let addEndOn fn (toil: Toil) =
  toil.AddEndCondition(new System.Func<JobCondition>(fn))
  toil

let addFailOnDestroyedNullOrForbidden targetIndex (toil: Toil) =
  toil.FailOnDestroyedNullOrForbidden(targetIndex)

let addFailOnCannotTouch targetIndex pathEndMode (toil: Toil) =
  toil.FailOnCannotTouch(targetIndex, pathEndMode)

let addFailOnSomeoneInteracting targetIndex (toil: Toil) =
  toil.FailOnSomeonePhysicallyInteracting(targetIndex)

let addProgressBar targetIndex (toil: Toil) =
  toil.WithProgressBarToilDelay(targetIndex)
