namespace Infusion

open RimWorld
open Verse
open Verse.AI

open VerseInterop
open VerseTools

module JobDriver =
    let hasNoInfusionToRemove (thing: Thing) =
        compOfThing<Comp.Infusion> thing
        |> Option.map (fun c -> c.RemovalSet.Count = 0)
        |> Option.defaultValue true

    let failOnNoRemovalSet (target: TargetIndex) (job: IJobEndable) =
        let thing =
            job.GetActor().jobs.curJob.GetTarget(target).Thing

        job.AddEndCondition(fun () ->
            if hasNoInfusionToRemove thing then JobCondition.Incompletable else JobCondition.Ongoing)

open JobDriver

type JobDriverRemoveInfusions() =
    inherit JobDriver()

    let worksToDo = 300.0f
    let mutable worksDone = 0.0f

    member this.GetWorksFraction() = worksDone / worksToDo

    override this.TryMakePreToilReservations(errorOnFailed) =
        this.pawn.Reserve(this.job.targetA, this.job, 1, -1, null, errorOnFailed)

    override this.MakeNewToils() =
        let thing = this.TargetThingA
        let targetComp = thing |> compOfThing<Comp.Infusion>

        do this.FailOnDestroyedNullOrForbidden(TargetIndex.A)
           |> failOnNoRemovalSet TargetIndex.A

        seq {
            yield Toils_Goto.GotoThing(TargetIndex.A, PathEndMode.ClosestTouch)
            yield Toil()
                  |> Toil.setTickAction (fun () ->
                      (do worksDone <-
                          worksDone
                          + this.pawn.GetStatValue(StatDefOf.GeneralLaborSpeed)
                       if worksDone >= worksToDo then
                           do targetComp
                              |> Option.iter (Comp.removeMarkedInfusions)
                           do this.ReadyForNextToil()))
                  |> Toil.addFailOnCannotTouch TargetIndex.A PathEndMode.ClosestTouch
                  |> Toil.addFailOnSomeoneInteracting TargetIndex.A
                  |> Toil.addFailOn (fun () -> hasNoInfusionToRemove thing)
                  |> Toil.setDefaultCompleteMode ToilCompleteMode.Never
                  |> Toil.addProgressBar TargetIndex.A this.GetWorksFraction
        }

    override this.ExposeData() =
        base.ExposeData()

        scribeValue "worksDone" worksDone
        |> Option.iter (fun a -> do worksDone <- a)
