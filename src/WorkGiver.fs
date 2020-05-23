namespace Infusion

open RimWorld
open Verse
open Verse.AI

open Lib
open VerseInterop
open VerseTools

module WorkGiver =
    let hasSameMap (a: Thing) (b: Thing) = a.Map = b.Map

    let chooseSameMapOnly target comps =
        comps
        |> Seq.map parentOfComp
        |> Seq.filter (hasSameMap target)
        |> Seq.map upcastToThing

open WorkGiver


type WorkGiverApplyInfuser() =
    inherit WorkGiver_Scanner()

    override this.PotentialWorkThingsGlobal pawn = chooseSameMapOnly pawn CompInfusion.WantingCandidates

    override this.JobOnThing(pawn, thing, forced) =
        let thingTarget = LocalTargetInfo(thing)

        let toInfuse =
            compOfThing<CompInfusion> thing
            |> Option.bind (fun comp ->
                match comp.FirstWanting with
                | Some a -> Some a
                | None ->
                    do CompInfusion.UnregisterWantingCandidates comp
                    None)

        let matchingInfuser =
            toInfuse
            |> Option.bind ((flip Map.tryFind) Infuser.AllInfusersByDef)
            |> Option.map LocalTargetInfo

        matchingInfuser
        |> Option.bind (fun infuser ->
            if pawn.CanReserve(thingTarget, 1, -1, null, forced)
               && pawn.CanReserve(infuser, 1, 1, null, forced) then
                Some(JobMaker.MakeJob(DefDatabase<JobDef>.GetNamed("Infusion_ApplyInfuser"), thingTarget, infuser))
            else
                do JobFailReason.Is(translate "Infusion.Job.FailReason.NoMatchingInfuser")
                None)
        |> Option.defaultValue null


type WorkGiverExtractInfusion() =
    inherit WorkGiver_Scanner()

    override this.PotentialWorkThingsGlobal pawn = chooseSameMapOnly pawn CompInfusion.ExtractionCandidates

    override this.JobOnThing(pawn, thing, forced) =
        let thingTarget = LocalTargetInfo(thing)

        let nearestExtractor =
            GenClosest.ClosestThing_Global_Reachable
                (pawn.Position,
                 pawn.Map,
                 Extractor.AllExtractors |> Seq.map upcastToThing,
                 PathEndMode.Touch,
                 TraverseParms.For(pawn),
                 validator =
                     fun thing ->
                         not (thing.IsForbidden(pawn))
                         && pawn.CanReserve(LocalTargetInfo(thing), 10, 1))
            |> Option.ofObj
            |> Option.map LocalTargetInfo

        nearestExtractor
        |> Option.bind (fun extractor ->
            if pawn.CanReserve(thingTarget, 1, -1, null, forced) then
                Some(JobMaker.MakeJob(DefDatabase<JobDef>.GetNamed("Infusion_ExtractInfusion"), thingTarget, extractor))
            else
                do JobFailReason.Is(translate "Infusion.Job.FailReason.NoExtractor")
                None)
        |> Option.defaultValue null


type WorkGiverRemoveInfusions() =
    inherit WorkGiver_Scanner()

    override this.PotentialWorkThingsGlobal pawn = chooseSameMapOnly pawn CompInfusion.RemovalCandidates

    override this.JobOnThing(pawn, thing, forced) =
        let thingTarget = LocalTargetInfo(thing)

        if pawn.CanReserve(thingTarget, 1, -1, null, forced)
        then JobMaker.MakeJob(DefDatabase<JobDef>.GetNamed("Infusion_RemoveInfusions"), thingTarget)
        else null
