namespace Infusion.OnHitWorkers

open RimWorld
open Verse

open Infusion
open Lib
open VerseTools


// [todo] Group as HediffBase
type ReduceSeverity =
    inherit OnHitWorker

    val mutable bodySizeMatters: bool
    val mutable def: HediffDef
    val mutable severityScaleBy: StatDef

    new() =
        { bodySizeMatters = true
          def = null
          severityScaleBy = null }

    member this.SeverityScaleBy = Option.ofObj this.severityScaleBy

    override this.MeleeHit record =
        if this.selfCast then
            this.ReduceSeverityBy record.baseDamage record.verb.CasterPawn
        else
            do tryCast<Pawn> record.target
               |> Option.iter (this.ReduceSeverityBy record.baseDamage)

    override this.BulletHit record =
        if this.selfCast then
            do tryCast<Pawn> record.projectile.Launcher
               |> Option.iter (this.ReduceSeverityBy record.baseDamage)
        else
            do record.target
               |> Option.bind tryCast<Pawn>
               |> Option.iter (this.ReduceSeverityBy record.baseDamage)

    member private this.ReduceSeverityBy baseDamage (pawn: Pawn) =
        if Pawn.isAliveAndWell pawn then
            let amount = baseDamage * this.amount

            do Option.ofObj (pawn.health.hediffSet.GetFirstHediffOfDef this.def)
               |> Option.iter (fun hediff -> do hediff.Heal(this.CalculateSeverity amount pawn))

    member private this.CalculateSeverity amount (pawn: Pawn) =
        let statScale =
            this.SeverityScaleBy
            |> Option.map pawn.GetStatValue
            |> Option.defaultValue 1.0f

        let bodySizeScale =
            if this.bodySizeMatters then pawn.BodySize else 1.0f

        amount * statScale / bodySizeScale / 100.0f
