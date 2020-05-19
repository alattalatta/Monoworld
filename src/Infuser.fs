namespace Infusion

open System
open Verse

open VerseInterop
open VerseTools


type CompInfuserProperties =
    inherit CompProperties

    val mutable forcedMaxCount: int
    val mutable forcedTier: TierDef

    new() =
        { inherit CompProperties(typeof<CompInfusion>)
          forcedMaxCount = Int32.MaxValue
          forcedTier = TierDef.empty }

    new(maxCount, tier) =
        { inherit CompProperties(typeof<CompInfusion>)
          forcedMaxCount = maxCount
          forcedTier = tier }


type Infuser() =
    inherit ThingWithComps()

    static let mutable allInfusers = Set.empty<Infuser>

    static member AllInfusers = allInfusers

    static member AllInfusersByDef =
        allInfusers
        |> Seq.choose (fun infuser ->
            match infuser.Content with
            | Some infDef -> Some(infDef, infuser)
            | _ -> None)
        |> Map.ofSeq

    member this.Content =
        compOfThing<CompInfusion> this
        |> Option.map (fun comp -> Seq.exactlyOne comp.Infusions)

    member this.SetContent inf =
        do compOfThing<CompInfusion> this
           |> Option.iter (fun comp -> do comp.Infusions <- seq { yield inf })
        do resetHP this

    override this.PostMake() =
        base.PostMake()

        do compOfThing<CompInfusion> this
           |> Option.filter (fun comp -> comp.Size = 0)
           |> Option.bind (fun comp ->
               match comp.props with
               | :? CompInfuserProperties as props ->
                   if props.forcedTier.defName = "UnnamedDef" then None else Some(comp, props)
               | _ -> None)
           |> Option.iter (fun (comp, props) ->
               do comp.Infusions <-
                   seq {
                       yield DefDatabase<InfusionDef>.AllDefs
                             |> Seq.filter (fun inf -> inf.tier = props.forcedTier)
                             |> GenCollection.RandomElement
                   })

    override this.SpawnSetup(map, respawningAfterLoad) =
        base.SpawnSetup(map, respawningAfterLoad)

        do allInfusers <- Set.add this allInfusers

    override this.DeSpawn(mode) =
        base.DeSpawn(mode)

        do allInfusers <- Set.remove this allInfusers

    override this.Equals(ob: obj) =
        match ob with
        | :? Thing as thing -> this.ThingID = thing.ThingID
        | _ -> false

    override this.GetHashCode() = base.GetHashCode()

    interface IComparable with
        member this.CompareTo(ob: obj) =
            match ob with
            | :? Thing as thing -> this.ThingID.CompareTo thing.ThingID
            | _ -> 0
