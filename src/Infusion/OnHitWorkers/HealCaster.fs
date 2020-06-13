namespace Infusion.OnHitWorkers

open Poet.Lib
open Verse

open Infusion
open VerseTools


// [todo] Associate with battle logs
type HealCaster() =
    inherit OnHitWorker()

    override this.MeleeHit record =
        let caster = record.verb.CasterPawn

        if Pawn.isAliveAndWell caster
        then do this.HealRandomInjury (record.baseDamage * this.amount) caster

    override this.BulletHit record =
        do tryCast<Pawn> record.projectile.Launcher
           |> Option.filter Pawn.isAliveAndWell
           |> Option.iter (this.HealRandomInjury(record.baseDamage * this.amount))

    member private this.HealRandomInjury amount (caster: Pawn) =
        let hediffSet = caster.health.hediffSet

        if hediffSet.HasNaturallyHealingInjury() then
            let injuries =
                hediffSet.GetHediffs<Hediff_Injury>()
                |> Seq.filter (fun hediff -> hediff.CanHealNaturally())

            do injuries.RandomElement().Heal(amount * caster.HealthScale)
