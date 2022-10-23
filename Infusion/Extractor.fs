namespace Infusion

open System
open Verse


type Extractor() =
  inherit ThingWithComps()

  static let mutable allExtractors = Set.empty<Extractor>

  static member AllExtractors = allExtractors

  override this.SpawnSetup(map, respawningAfterLoad) =
    ``base``.SpawnSetup(map, respawningAfterLoad)

    do allExtractors <- Set.add this allExtractors

  override this.DeSpawn(mode) =
    ``base``.DeSpawn(mode)

    do allExtractors <- Set.remove this allExtractors

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
