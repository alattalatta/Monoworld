namespace Infusion

open Poet.Lib
open Poet.Lyric
open Poet.Lyric.Thing
open Poet.Lyric.Translation
open RimWorld
open Verse
open Verse.AI


module JobDriver =
  let incompletableOnSetEmpty set =
    if Set.isEmpty set then
      JobCondition.Incompletable
    else
      JobCondition.Ongoing

  let errorOnNone opt =
    match opt with
    | Some _ -> JobCondition.Ongoing
    | None -> JobCondition.Errored

  let compInfusionOfTarget (job: IJobEndable) target =
    job.GetActor().jobs.curJob.GetTarget(target).Thing
    |> Comp.ofThing<CompInfusion>

  let waitFor delay target =
    Toils_General.WaitWith(target, delay, true)

  let carryBToA (job: Job) =
    seq {
      yield Toils_General.DoAtomic(fun () -> do job.count <- 1)

      yield
        Toils_Goto
          .GotoThing(TargetIndex.B, PathEndMode.Touch)
          .FailOnSomeonePhysicallyInteracting(TargetIndex.B)

      yield
        Toils_Haul
          .StartCarryThing(TargetIndex.B)
          .FailOnDestroyedNullOrForbidden(TargetIndex.B)

      yield Toils_Goto.GotoThing(TargetIndex.A, PathEndMode.Touch)
    }

  let destroyCarriedThing (pawn: Pawn) =
    Option.ofObj pawn.carryTracker.CarriedThing
    |> Option.iter (fun t -> t.Destroy())


  module ApplyInfuser =
    let failOnNoWantingSet target (job: IJobEndable) =
      do
        job.AddEndCondition (fun () ->
          compInfusionOfTarget job target
          |> Option.map (fun c -> c.WantingSet)
          |> Option.defaultValue Set.empty
          |> incompletableOnSetEmpty)

      job

    let failOnNoMatchingInfuser target (job: IJobEndable) =
      do
        job.AddEndCondition (fun () ->
          compInfusionOfTarget job target
          |> Option.bind (fun c -> c.FirstWanting)
          |> Option.bind ((flip Map.tryFind) Infuser.AllInfusersByDef)
          |> errorOnNone)

      job


  module ExtractInfusion =
    let failOnNoExtractionSet target (job: IJobEndable) =
      do
        job.AddEndCondition (fun () ->
          compInfusionOfTarget job target
          |> Option.map (fun c -> c.ExtractionSet)
          |> Option.defaultValue Set.empty
          |> incompletableOnSetEmpty)

      job


  module RemoveInfusions =
    let failOnNoRemovalSet target (job: IJobEndable) =
      do
        job.AddEndCondition (fun () ->
          compInfusionOfTarget job target
          |> Option.map (fun c -> c.RemovalSet)
          |> Option.defaultValue Set.empty
          |> incompletableOnSetEmpty)

      job

open JobDriver


type JobDriverApplyInfuser() =
  inherit JobDriver()

  override this.TryMakePreToilReservations(errorOnFailed) =
    this.pawn.Reserve(this.job.targetA, this.job, 1, -1, null, errorOnFailed)
    && this.pawn.Reserve(this.job.targetB, this.job, 1, 1, null, errorOnFailed)

  override this.MakeNewToils() =
    let target = this.TargetThingA
    let targetComp = target |> Comp.ofThing<CompInfusion>

    // must... be... an Infuser...
    let infuser = this.TargetThingB :?> Infuser

    do
      this
        .FailOnDestroyedNullOrForbidden(TargetIndex.A)
        .FailOnDestroyedNullOrForbidden(TargetIndex.B)
      |> ApplyInfuser.failOnNoWantingSet TargetIndex.A
      |> ignore

    seq {
      yield! carryBToA this.job

      yield
        waitFor 300 TargetIndex.A
        |> Toil.addFailOnDestroyedNullOrForbidden TargetIndex.A
        |> Toil.addEndOn (fun () ->
          targetComp
          |> Option.map (fun comp -> incompletableOnSetEmpty comp.WantingSet)
          |> errorOnNone)

      yield
        Toils_General.Do (fun () ->
          targetComp
          |> Option.bind (fun comp ->
            infuser.Content
            |> Option.map (fun inf -> (comp, inf)))
          |> Option.tap (fun (comp, inf) -> do CompInfusion.addInfusion inf comp)
          // with reusableInfusers turned on, place a new empty infuser
          |> Option.tap (fun _ ->
            if Settings.ReusableInfusers.handle.Value then
              ThingMaker.MakeThing(ThingDef.Named("Infusion_InfuserEmpty"))
              |> placeThingNear this.pawn.Position this.pawn.Map
              |> ignore)
          |> Option.iter (fun _ -> do destroyCarriedThing this.pawn))
    }


type JobDriverExtractInfusion() =
  inherit JobDriver()

  override this.TryMakePreToilReservations(errorOnFailed) =
    this.pawn.Reserve(this.job.targetA, this.job, 1, -1, null, errorOnFailed)
    && this.pawn.Reserve(this.job.targetB, this.job, 10, 1, null, errorOnFailed)

  override this.MakeNewToils() =
    let target = this.TargetThingA
    let targetComp = target |> Comp.ofThing<CompInfusion>

    let extractor = this.TargetThingB

    do
      this
        .FailOnDestroyedNullOrForbidden(TargetIndex.A)
        .FailOnDestroyedNullOrForbidden(TargetIndex.B)
      |> ExtractInfusion.failOnNoExtractionSet TargetIndex.A
      |> ignore

    seq {
      yield! carryBToA this.job

      yield
        waitFor 300 TargetIndex.A
        |> Toil.addFailOnDestroyedNullOrForbidden TargetIndex.A
        |> Toil.addEndOn (fun () ->
          targetComp
          |> Option.map (fun comp -> incompletableOnSetEmpty comp.ExtractionSet)
          |> errorOnNone)

      yield
        Toils_General.Do (fun () ->
          targetComp
          |> Option.bind (fun comp ->
            comp.FirstExtraction
            |> Option.map (fun inf -> (comp, inf)))
          |> Option.iter (fun (comp, inf) ->
            let baseExtractionChance =
              inf.tier.extractionChance
              * Settings.ExtractionChanceFactor.handle.Value

            let successChance =
              comp.Biocoder
              |> Option.map (fun _ -> baseExtractionChance * 0.5f)
              |> Option.defaultValue baseExtractionChance

            if Rand.Chance successChance then
              let infuser =
                ThingMaker.MakeThing(ThingDef.Named("Infusion_Infuser_" + inf.tier.defName)) :?> Infuser

              do
                CompInfusion.removeInfusion inf comp
                infuser.SetContent inf

              infuser
              |> placeThingNear this.pawn.Position this.pawn.Map
              |> Option.iter (fun _ -> destroyCarriedThing this.pawn)
            else
              let chance = Rand.Value

              let failureMessage =
                if chance >= 0.5f then
                  do CompInfusion.removeInfusion inf comp
                  "Infusion.Job.Message.ExtractionFailureInfusion"
                elif chance >= 0.2f then
                  do destroyCarriedThing this.pawn
                  "Infusion.Job.Message.ExtractionFailureInfuser"
                else
                  do
                    CompInfusion.removeInfusion inf comp
                    destroyCarriedThing this.pawn

                  "Infusion.Job.Message.ExtractionFailure"

              Messages.Message(
                string (translate2 failureMessage inf.label target.def.label),
                LookTargets(extractor),
                MessageTypeDefOf.NegativeEvent
              )))
    }


type JobDriverRemoveInfusions() =
  inherit JobDriver()

  override this.TryMakePreToilReservations(errorOnFailed) =
    this.pawn.Reserve(this.job.targetA, this.job, 1, -1, null, errorOnFailed)

  override this.MakeNewToils() =
    let target = this.TargetThingA
    let targetComp = target |> Comp.ofThing<CompInfusion>

    do
      this.FailOnDestroyedNullOrForbidden(TargetIndex.A)
      |> RemoveInfusions.failOnNoRemovalSet TargetIndex.A
      |> ignore

    seq {
      yield Toils_Goto.GotoThing(TargetIndex.A, PathEndMode.Touch)

      yield
        waitFor 1000 TargetIndex.A
        |> Toil.addFailOnDestroyedNullOrForbidden TargetIndex.A
        |> Toil.addEndOn (fun () ->
          targetComp
          |> Option.map (fun comp -> incompletableOnSetEmpty comp.RemovalSet)
          |> errorOnNone)

      yield
        Toils_General.Do (fun () ->
          do
            targetComp
            |> Option.iter CompInfusion.removeMarkedInfusions)
    }
