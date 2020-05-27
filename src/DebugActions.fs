module Infusion.DebugActions

open System

open Verse

open Lib
open VerseInterop
open RimWorld

let private pointedThings () =
    Find.CurrentMap.thingGrid.ThingsAt(UI.MouseCell())

let private firstCompAtPointer<'T when 'T :> ThingComp and 'T: null> (things: seq<Thing>) =
    Seq.tryFind (fun thing -> compOfThing<'T> thing |> Option.isSome) things
    |> Option.bind compOfThing<'T>

let private addActionFor (infDef: InfusionDef) =
    pointedThings ()
    |> firstCompAtPointer
    |> Option.iter (CompInfusion.addInfusion infDef)

let allQualities =
    seq {
        for qc in Enum.GetValues(typeof<QualityCategory>) do
            yield qc :?> QualityCategory
    }


[<DebugAction("Infusion", "Force set quality...", actionType = DebugActionType.ToolMap)>]
let forceSetQuality () =
    pointedThings ()
    |> firstCompAtPointer<CompQuality>
    |> Option.map (fun comp ->
        allQualities
        |> Seq.map (fun qc ->
            DebugMenuOption
                (Enum.GetName(typeof<QualityCategory>, qc),
                 DebugMenuOptionMode.Action,
                 (fun () -> do comp.SetQuality(qc, ArtGenerationContext.Colony)))))
    |> Option.map Dialog_DebugOptionListLister
    |> Option.iter Find.WindowStack.Add


[<DebugAction("Infusion", "Add an infusion to...", actionType = DebugActionType.Action)>]
let addInfusion () =
    DefDatabase<InfusionDef>.AllDefs
    |> Seq.filter InfusionDef.activeForUse
    |> Seq.sort
    |> Seq.map (fun infDef -> DebugMenuOption(infDef.defName, DebugMenuOptionMode.Tool, (fun () -> addActionFor infDef)))
    |> Dialog_DebugOptionListLister
    |> Find.WindowStack.Add


[<DebugAction("Infusion", "Remove an infusion from...", actionType = DebugActionType.ToolMap)>]
let removeInfusion () =
    pointedThings ()
    |> firstCompAtPointer<CompInfusion>
    |> Option.map (fun comp ->
        comp.Infusions
        |> Seq.map (fun infDef ->
            DebugMenuOption
                (infDef.defName, DebugMenuOptionMode.Action, (fun () -> do comp |> CompInfusion.removeInfusion infDef))))
    |> Option.map Dialog_DebugOptionListLister
    |> Option.iter Find.WindowStack.Add


[<DebugAction("Infusion", "Remove all infusions from...", actionType = DebugActionType.ToolMap)>]
let removeAllInfusions () =
    pointedThings ()
    |> firstCompAtPointer
    |> Option.iter CompInfusion.removeAllInfusions


[<DebugAction("Infusion", "Reroll infusions of...", actionType = DebugActionType.ToolMap)>]
let rerollInfusions () =
    pointedThings ()
    |> firstCompAtPointer
    |> Option.iter CompInfusion.rerollInfusions


[<DebugAction("Infusion",
              "Reroll everything in current map",
              actionType = DebugActionType.Action,
              allowedGameStates = AllowedGameStates.PlayingOnMap)>]
let rerollEverything () =
    let thingLister = Find.CurrentMap.listerThings

    thingLister.ThingsInGroup(ThingRequestGroup.Weapon)
    |> Seq.append (thingLister.ThingsInGroup(ThingRequestGroup.Apparel))
    |> Seq.choose compOfThing<CompInfusion>
    |> Seq.filter (fun comp -> comp.SlotCount > 0)
    |> Seq.iter CompInfusion.rerollInfusions
