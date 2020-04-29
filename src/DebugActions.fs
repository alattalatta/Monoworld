module Infusion.DebugActions

open Verse

open Lib
open VerseInterop

let private pointedThings() = Find.CurrentMap.thingGrid.ThingsAt(UI.MouseCell())

let private firstCompAtPointer (things: seq<Thing>) =
    Seq.tryFind (fun thing -> compOfThing<Comp.Infusion> thing |> Option.isSome) things
    |> Option.bind compOfThing<Comp.Infusion>

let private addActionFor (infDef: InfusionDef) =
    pointedThings()
    |> firstCompAtPointer
    |> Option.iter (tap (Comp.addInfusion infDef) >> Comp.resetHP)

[<DebugAction("Infusion", "Add an infusion...", actionType = DebugActionType.Action,
              allowedGameStates = AllowedGameStates.PlayingOnMap)>]
let addInfusion() =
    DefDatabase<InfusionDef>.AllDefs
    |> Seq.filter (fun def -> not def.disabled)
    |> Seq.sort
    |> Seq.map
        (fun infDef -> DebugMenuOption(infDef.defName, DebugMenuOptionMode.Tool, (fun () -> addActionFor infDef)))
    |> Dialog_DebugOptionListLister
    |> Find.WindowStack.Add

[<DebugAction("Infusion", "Remove an infusion...", actionType = DebugActionType.ToolMap)>]
let removeInfusion() =
    pointedThings()
    |> firstCompAtPointer
    |> Option.map (fun comp ->
        comp.Infusions
        |> Seq.map (fun infDef ->
            DebugMenuOption
                (infDef.defName, DebugMenuOptionMode.Action,
                 (fun () ->
                     comp
                     |> tap (Comp.removeInfusion infDef)
                     |> Comp.resetHP))))
    |> Option.map Dialog_DebugOptionListLister
    |> Option.iter Find.WindowStack.Add

[<DebugAction("Infusion", "Remove all infusions", actionType = DebugActionType.ToolMap)>]
let removeAllInfusions() =
    pointedThings()
    |> firstCompAtPointer
    |> Option.iter ((tap Comp.removeAllInfusions) >> Comp.resetHP)

[<DebugAction("Infusion", "Reroll infusions", actionType = DebugActionType.ToolMap)>]
let rerollInfusions() =
    pointedThings()
    |> firstCompAtPointer
    |> Option.iter (fun comp ->
        do comp.Infusions <- Comp.pickInfusions comp.Quality comp.parent
        do Comp.resetHP comp)
