namespace Infusion

open RimWorld
open Verse


type StockGeneratorInfuser =
  inherit StockGenerator

  val mutable tierPriorityLimit: int

  new() =
    { inherit StockGenerator()
      tierPriorityLimit = 0 }

  member this.RandomCountFor(infuser: ThingDef) = this.RandomCountOf(infuser)

  override this.GenerateThings(_, faction) =
    DefDatabase<TierDef>.AllDefs
    |> Seq.filter (Settings.Tiers.isEnabled)
    |> Seq.filter (fun tier -> tier.priority <= this.tierPriorityLimit)
    |> Seq.collect (fun tier ->
      StockGeneratorUtility.TryMakeForStock(tier.infuser, this.RandomCountFor(tier.infuser), faction))

  override this.HandlesThingDef(thingDef) =
    Option.ofObj thingDef.tradeTags
    |> Option.map (fun tags -> tags.Contains "Infusion_Infuser")
    |> Option.defaultValue false
