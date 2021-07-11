namespace Infusion

open Poet.Lib
open RimWorld
open Verse


type StockGeneratorInfuser =
    inherit StockGenerator

    val mutable tierPriorityLimit: int

    new() =
        { inherit StockGenerator()
          tierPriorityLimit = 0 }

    member this.RandomCountFor(infuser: ThingDef) = this.RandomCountOf(infuser)

    override this.GenerateThings(_, _) =
        DefDatabase<TierDef>.AllDefs
        |> Seq.filter (
            (Settings.Tiers.isEnabled
             <&> (TierDef.priority >> ((>) this.tierPriorityLimit)))
        )
        |> Seq.map
            (fun tier ->
                StockGeneratorUtility.TryMakeForStockSingle(tier.infuser, this.RandomCountFor(tier.infuser), null))

    override this.HandlesThingDef(thingDef) =
        Option.ofObj thingDef.tradeTags
        |> Option.map (fun tags -> tags.Contains "Infusion_Infuser")
        |> Option.defaultValue false
