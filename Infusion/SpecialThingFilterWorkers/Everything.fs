// Def can't find types within modules, e.g.
// module Infusion.SpecialThingFilterWorkers
// so this must not be a module declaration
namespace Infusion.SpecialThingFilterWorkers

open Poet.Lyric
open Verse

open Infusion


type BaseFilterWorker(flag: bool) =
    inherit SpecialThingFilterWorker()

    let matchAgainst flag thing =
        flag =
            (Comp.ofThing<CompInfusion> thing
             |> Option.map (fun comp -> comp.Size > 0)
             |> Option.defaultValue false)

    override this.Matches thing =
        this.CanEverMatch thing.def
        && matchAgainst flag thing

type InfusedApparels() =
    inherit BaseFilterWorker(true)

    override this.CanEverMatch thingDef = thingDef.IsApparel

type NonInfusedApparels() =
    inherit BaseFilterWorker(false)

    override this.CanEverMatch thingDef = thingDef.IsApparel

type InfusedWeapons() =
    inherit BaseFilterWorker(true)

    override this.CanEverMatch thingDef = thingDef.IsWeapon

type NonInfusedWeapons() =
    inherit BaseFilterWorker(false)

    override this.CanEverMatch thingDef = thingDef.IsWeapon
