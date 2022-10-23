namespace Infusion

open System

open Poet.Lyric
open Verse

open VerseTools


type CompInfuserProperties =
  inherit CompProperties

  val mutable forcedTier: TierDef

  new() =
    { inherit CompProperties(typeof<CompInfusion>)
      forcedTier = TierDef.empty }

  new(tier) =
    { inherit CompProperties(typeof<CompInfusion>)
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
    Comp.ofThing<CompInfusion> this
    |> Option.map (fun comp -> Seq.exactlyOne comp.Infusions)

  member this.SetContent inf =
    do
      Comp.ofThing<CompInfusion> this
      |> Option.iter (fun comp -> do comp.Infusions <- seq { yield inf })

    do resetHP this

  override this.PostMake() =
    base.PostMake()

    do
      Comp.ofThing<CompInfusion> this
      |> Option.filter (fun comp -> comp.Size = 0)
      |> Option.bind (fun comp ->
        match comp.props with
        | :? CompInfuserProperties as props ->
          if props.forcedTier.defName = "UnnamedDef" then
            None
          else
            Some(comp, props)
        | _ -> None)
      |> Option.iter (fun (comp, props) ->
        do
          comp.Infusions <-
            seq {
              yield
                DefDatabase<InfusionDef>.AllDefs
                |> Seq.filter (fun inf -> inf.tier = props.forcedTier)
                |> Seq.filter InfusionDef.activeForUse
                |> GenCollection.RandomElement
            })

  override this.SpawnSetup(map, respawningAfterLoad) =
    ``base``.SpawnSetup(map, respawningAfterLoad)

    do allInfusers <- Set.add this allInfusers

  override this.DeSpawn(mode) =
    ``base``.DeSpawn(mode)

    do allInfusers <- Set.remove this allInfusers

  override this.SplitOff(count) =
    let otherOne = ``base``.SplitOff(count)

    do
      this.Content
      |> Option.bind (fun inf ->
        Comp.ofThing<CompInfusion> otherOne
        |> Option.map (fun comp -> (comp, inf)))
      |> Option.iter (fun (comp, inf) -> do comp.Infusions <- seq { yield inf })

    otherOne

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
