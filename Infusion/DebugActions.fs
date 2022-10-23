module Infusion.DebugActions

open System

open Poet.Lib
open Poet.Lyric
open Verse

open RimWorld

let private listAllQualities () =
  [ for qc in Enum.GetValues(typeof<QualityCategory>) do
      yield qc :?> QualityCategory ]

let private spawnThingOfQuality (def: ThingDef) (qc: QualityCategory) =
  let stuff = GenStuff.RandomStuffFor(def)
  let thing = ThingMaker.MakeThing(def, stuff)

  thing
  |> Comp.ofThing<CompQuality>
  |> Option.iter (fun comp -> comp.SetQuality(qc, ArtGenerationContext.Colony))

  GenPlace.TryPlaceThing(thing, UI.MouseCell(), Find.CurrentMap, ThingPlaceMode.Near)
  |> ignore

let openDebugOptionsLister options =
  options
  |> Dialog_DebugOptionListLister
  |> Find.WindowStack.Add

let private equipmentSelector predicate =
  DefsForReading.allThingsInfusable
  |> List.filter predicate
  |> List.map (fun def ->
    let onClick =
      (fun () ->
        listAllQualities ()
        |> List.map (fun qc ->
          DebugMenuOption(
            Enum.GetName(typeof<QualityCategory>, qc),
            DebugMenuOptionMode.Tool,
            (fun () -> (spawnThingOfQuality def qc))
          ))
        |> openDebugOptionsLister)

    DebugMenuOption(def.defName, DebugMenuOptionMode.Action, Action(onClick)))


[<DebugAction("Infusion",
              "Spawn weapon with quality...",
              actionType = DebugActionType.Action,
              allowedGameStates = AllowedGameStates.PlayingOnMap)>]
let spawnWeaponWithQuality () =
  equipmentSelector (fun def -> def.IsWeapon)
  |> openDebugOptionsLister


[<DebugAction("Infusion",
              "Spawn apparel with quality...",
              actionType = DebugActionType.Action,
              allowedGameStates = AllowedGameStates.PlayingOnMap)>]
let spawnApparelWithQuality () =
  equipmentSelector (fun def -> def.IsApparel)
  |> openDebugOptionsLister


let private pointedThings () =
  Find.CurrentMap.thingGrid.ThingsAt(UI.MouseCell())


let private firstCompAtPointer<'T when 'T :> ThingComp and 'T: null> (things: seq<Thing>) =
  Seq.tryFind (fun thing -> Comp.ofThing<'T> thing |> Option.isSome) things
  |> Option.bind Comp.ofThing<'T>


let private infusePointed (infDef: InfusionDef) =
  pointedThings ()
  |> firstCompAtPointer
  |> Option.iter (CompInfusion.addInfusion infDef)


[<DebugAction("Infusion", "Infuse...", actionType = DebugActionType.Action)>]
let addInfusion () =
  DefDatabase<InfusionDef>.AllDefs
  |> Seq.filter InfusionDef.activeForUse
  |> Seq.sort
  |> Seq.map (fun infDef -> DebugMenuOption(infDef.defName, DebugMenuOptionMode.Tool, (fun () -> infusePointed infDef)))
  |> openDebugOptionsLister


[<DebugAction("Infusion", "Remove an infusion...", actionType = DebugActionType.ToolMap)>]
let removeInfusion () =
  pointedThings ()
  |> firstCompAtPointer<CompInfusion>
  |> Option.map (fun comp ->
    comp.Infusions
    |> Seq.map (fun infDef ->
      DebugMenuOption(
        infDef.defName,
        DebugMenuOptionMode.Action,
        (fun () -> do comp |> CompInfusion.removeInfusion infDef)
      )))
  |> Option.iter openDebugOptionsLister


[<DebugAction("Infusion", "Remove all infusions", actionType = DebugActionType.ToolMap)>]
let removeAllInfusions () =
  pointedThings ()
  |> firstCompAtPointer
  |> Option.iter CompInfusion.removeAllInfusions


[<DebugAction("Infusion", "Reroll infusions", actionType = DebugActionType.ToolMap)>]
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
  |> Seq.choose Comp.ofThing<CompInfusion>
  |> Seq.filter (fun comp -> comp.SlotCount > 0)
  |> Seq.iter CompInfusion.rerollInfusions
