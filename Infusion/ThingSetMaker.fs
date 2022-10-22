namespace Infusion

open Poet.Lib
open RimWorld
open Verse


type ThingSetMakerInfuser() =
    inherit ThingSetMaker()

    member this.GenerateOne() =
        DefDatabase<TierDef>.AllDefs
        |> Seq.filter (Settings.Tiers.isEnabled)
        |> Option.ofSeq
        |> Option.map GenCollection.RandomElement
        |> Option.map (fun tier -> ThingMaker.MakeThing tier.infuser)

    override this.Generate(param, outThings) =
        let count =
            Option.ofNullable param.countRange
            |> Option.map (fun range -> range.RandomInRange)
            |> Option.defaultValue 1

        let infusers =
            seq {
                for _ in 1 .. count do
                    yield this.GenerateOne()
            }
            |> Seq.choose id

        do outThings.AddRange infusers

    override this.AllGeneratableThingsDebugSub(_) =
        DefDatabase<TierDef>.AllDefs
        |> Seq.filter (Settings.Tiers.isEnabled)
        |> Seq.map (fun tier -> tier.infuser)
