namespace Infusion

open Poet.Lib
open Poet.Lyric
open Poet.Lyric.Translation
open RimWorld
open Verse
open Verse.AI


module JobDriver =
    let incompletableOnSetEmpty set =
        if Set.count set = 0 then JobCondition.Incompletable else JobCondition.Ongoing

    let incompletableOnNone opt =
        match opt with
        | Some _ -> JobCondition.Ongoing
        | None -> JobCondition.Incompletable

    let getTargetThing (job: IJobEndable) target =
        job.GetActor().jobs.curJob.GetTarget(target).Thing

    let getCompInfusionOfTaraget job target =
        getTargetThing job target
        |> Comp.ofThing<CompInfusion>

    let carryBToA (job: Job) =
        seq {
            yield Toils_General.DoAtomic(fun () -> do job.count <- 1)
            yield Toils_Goto.GotoThing(TargetIndex.B, PathEndMode.Touch)
                  |> Toil.addFailOnSomeoneInteracting TargetIndex.B
            yield Toils_Haul.StartCarryThing(TargetIndex.B)

            yield Toils_Goto.GotoThing(TargetIndex.A, PathEndMode.Touch)
        }

    let destroyCarriedThing (pawn: Pawn) =
        do Option.ofObj pawn.carryTracker.CarriedThing
           |> Option.iter (fun t -> t.Destroy())


    module ApplyInfuser =
        let failOnNoWantingSet target (job: IJobEndable) =
            let wantingSet =
                getCompInfusionOfTaraget job target
                |> Option.map (fun c -> c.WantingSet)
                |> Option.defaultValue Set.empty

            job.AddEndCondition(fun () -> incompletableOnSetEmpty wantingSet)
            job

        let failOnNoMatchingInfuser target (job: IJobEndable) =
            let matchingInfuser =
                getCompInfusionOfTaraget job target
                |> Option.bind (fun c -> c.FirstWanting)
                |> Option.bind ((flip Map.tryFind) Infuser.AllInfusersByDef)

            job.AddEndCondition(fun () -> incompletableOnNone matchingInfuser)
            job


    module ExtractInfusion =
        let failOnNoExtractionSet target (job: IJobEndable) =
            let extractionSet =
                getCompInfusionOfTaraget job target
                |> Option.map (fun c -> c.ExtractionSet)
                |> Option.defaultValue Set.empty

            job.AddEndCondition(fun () -> incompletableOnSetEmpty extractionSet)
            job


    module RemoveInfusions =
        let failOnNoRemovalSet target (job: IJobEndable) =
            let removalSet =
                getTargetThing job target
                |> Comp.ofThing<CompInfusion>
                |> Option.map (fun c -> c.RemovalSet)
                |> Option.defaultValue Set.empty

            job.AddEndCondition(fun () -> incompletableOnSetEmpty removalSet)
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

        do this.FailOnDestroyedNullOrForbidden(TargetIndex.A).FailOnDestroyedNullOrForbidden(TargetIndex.B)
           |> ApplyInfuser.failOnNoWantingSet TargetIndex.A
           |> ignore

        seq {
            yield! carryBToA this.job

            yield Toils_General.Wait(300)
                  |> Toil.addFailOnCannotTouch TargetIndex.A PathEndMode.Touch
                  |> Toil.addFailOnSomeoneInteracting TargetIndex.A
                  |> Toil.setTickAction (fun () ->
                      // fails when wanted set changes into empty,
                      // or slots somehow become full (dev tool?)
                      match targetComp with
                      | Some comp ->
                          if Set.count comp.WantingSet = 0
                             || Set.count comp.InfusionsRaw >= comp.SlotCount then
                              do this.EndJobWith(JobCondition.Incompletable)
                      | None -> do this.EndJobWith(JobCondition.Errored))
            yield Toils_General.Do(fun () ->
                      targetComp
                      |> Option.bind (fun comp ->
                          infuser.Content
                          |> Option.map (fun inf -> (comp, inf)))
                      |> Option.tap (fun (comp, inf) -> do CompInfusion.addInfusion inf comp)
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

        do this.FailOnDestroyedNullOrForbidden(TargetIndex.A).FailOnDestroyedNullOrForbidden(TargetIndex.B)
           |> ExtractInfusion.failOnNoExtractionSet TargetIndex.A
           |> ignore

        seq {
            yield! carryBToA this.job

            yield Toils_General.Wait(300)
                  |> Toil.addFailOnCannotTouch TargetIndex.A PathEndMode.Touch
                  |> Toil.addFailOnSomeoneInteracting TargetIndex.A
                  |> Toil.setTickAction (fun () ->
                      // fails when wanted set changes into empty,
                      // or slots somehow become full (dev tool?)
                      match targetComp with
                      | Some comp ->
                          if Set.count comp.ExtractionSet = 0
                          then do this.EndJobWith(JobCondition.Incompletable)
                      | None -> do this.EndJobWith(JobCondition.Errored))
            yield Toils_General.Do(fun () ->
                      targetComp
                      |> Option.bind (fun comp ->
                          comp.FirstExtraction
                          |> Option.map (fun inf -> (comp, inf)))
                      |> Option.tap (fun (comp, inf) -> do CompInfusion.removeInfusion inf comp)
                      |> Option.iter (fun (comp, inf) ->
                          let successChance =
                              comp.Biocoder
                              |> Option.map (fun _ -> inf.tier.extractionChance * 0.5f)
                              |> Option.defaultValue inf.tier.extractionChance

                          if Rand.Chance successChance then
                              let infuser =
                                  ThingMaker.MakeThing(ThingDef.Named("Infusion_Infuser_" + inf.tier.defName)) :?> Infuser

                              do infuser.SetContent inf

                              if GenPlace.TryPlaceThing(infuser, this.pawn.Position, this.pawn.Map, ThingPlaceMode.Near)
                              then do destroyCarriedThing this.pawn
                          else
                              Messages.Message
                                  (string
                                      (translate2 "Infusion.Job.Message.ExtractionFailure" inf.label target.def.label),
                                   LookTargets(extractor),
                                   MessageTypeDefOf.NegativeEvent)
                              do destroyCarriedThing this.pawn))
        }


type JobDriverRemoveInfusions() =
    inherit JobDriver()

    override this.TryMakePreToilReservations(errorOnFailed) =
        this.pawn.Reserve(this.job.targetA, this.job, 1, -1, null, errorOnFailed)

    override this.MakeNewToils() =
        let target = this.TargetThingA
        let targetComp = target |> Comp.ofThing<CompInfusion>

        do this.FailOnDestroyedNullOrForbidden(TargetIndex.A)
           |> RemoveInfusions.failOnNoRemovalSet TargetIndex.A
           |> ignore

        seq {
            yield Toils_Goto.GotoThing(TargetIndex.A, PathEndMode.Touch)

            yield Toils_General.Wait(1000)
                  |> Toil.addFailOnCannotTouch TargetIndex.A PathEndMode.Touch
                  |> Toil.addFailOnSomeoneInteracting TargetIndex.A
                  |> Toil.addProgressBar TargetIndex.A
                  |> Toil.setTickAction (fun () ->
                      match targetComp with
                      | Some comp ->
                          if Set.count comp.RemovalSet = 0
                          then do this.EndJobWith(JobCondition.Incompletable)
                      | None -> do this.EndJobWith(JobCondition.Errored))
            yield Toils_General.Do(fun () ->
                      do targetComp
                         |> Option.iter CompInfusion.removeMarkedInfusions)
        }
