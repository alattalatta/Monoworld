namespace Infusion.OnHitWorkers

open RimWorld
open Verse

open Infusion
open Lib
open VerseInterop
open VerseTools


type ApplyHediff =
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
            this.AddHediff record.baseDamage record.verb.CasterPawn
        else
            do tryCast<Pawn> record.target
               |> Option.iter (this.AddHediff record.baseDamage)

    override this.BulletHit record =
        if this.selfCast then
            do tryCast<Pawn> record.projectile.Launcher
               |> Option.iter (this.AddHediff record.baseDamage)
        else
            do record.target
               |> Option.bind tryCast<Pawn>
               |> Option.iter (this.AddHediff record.baseDamage)

    member private this.AddHediff baseDamage (pawn: Pawn) =
        if Pawn.isAliveAndWell pawn then
            let amount = baseDamage * this.amount
            let hediff = HediffMaker.MakeHediff(this.def, pawn)

            match compOfHediff<HediffComp_Disappears> hediff with
            | Some comp -> do comp.ticksToDisappear <- GenTicks.SecondsToTicks amount
            | _ -> do hediff.Severity <- this.CalculateSeverity amount pawn

            do pawn.health.AddHediff hediff

    member private this.CalculateSeverity amount (pawn: Pawn) =
        let statScale =
            this.SeverityScaleBy
            |> Option.map pawn.GetStatValue
            |> Option.defaultValue 1.0f

        let bodySizeScale =
            if this.bodySizeMatters then pawn.BodySize else 1.0f

        amount * statScale / bodySizeScale / 100.0f
