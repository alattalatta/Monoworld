// Def can't find the types when used with
// module Infusion.SpecialThingFilterWorkers
// so this must not be a module declaration
namespace Infusion.SpecialThingFilterWorkers

open Verse

open Infusion
open VerseInterop

type BaseFilterWorker(flag: bool) =
    inherit SpecialThingFilterWorker()

    let matchAgainst flag thing =
        flag =
            (compOfThing<Comp.Infusion> thing
             |> Option.map (fun comp -> comp.Size > 0)
             |> Option.defaultValue false)

    override this.Matches thing = this.CanEverMatch thing.def && matchAgainst flag thing

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
