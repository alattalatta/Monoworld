namespace Infusion

open RimWorld
open Verse
open Verse.AI

type WorkGiverRemoveInfusions() =
    inherit WorkGiver_Scanner()

    override this.PotentialWorkThingsGlobal pawn =
        Comp.Infusion.RemovalCandidates
        |> Seq.filter (fun comp -> comp.parent.Map = pawn.Map)
        |> Seq.map (fun comp -> comp.parent :> Thing)

    override this.JobOnThing(pawn, thing, forced) =
        let thingTargetInfo = LocalTargetInfo(thing)

        if pawn.CanReserve(thingTargetInfo, 1, -1, null, forced)
           && not (thing.IsForbidden(pawn))
           && not (thing.IsBurning()) then
            JobMaker.MakeJob(DefDatabase<JobDef>.GetNamed("Infusion_RemoveInfusions"), thingTargetInfo)
        else
            null
