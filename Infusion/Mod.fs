module Infusion.Mod

open System

open RimWorld
open Verse

open DefGenerator


[<StaticConstructorOnStartup>]
type StartupConstructor() =
  static do
    makeInfuserDefs ()
    |> Seq.iter (fun def ->
      do def.ResolveReferences()
      do DefGenerator.AddImpliedDef<ThingDef>(def)

      do
        Option.ofObj def.thingCategories
        |> Option.iter (Seq.iter (fun cat -> cat.childThingDefs.Add def))

      do HugsLib.Utils.InjectedDefHasher.GiveShortHashToDef(def, typeof<ThingDef>))


type ModBase() =
  inherit HugsLib.ModBase()

  override this.ModIdentifier = "latta.infusion"

  override this.SceneLoaded _ =
    CompInfusion.ClearCaches()

  override this.DefsLoaded() =
    do Settings.initialize ()
    do this.Inject()

  member private this.Inject() =
    this.InjectToThings()
    this.InjectToStats()

  member private this.InjectToThings() =
    let iTabType = typedefof<ITab.Infused>
    let iTab = InspectTabManager.GetSharedInstance(iTabType)

    DefsForReading.allThingsInfusable
    |> List.iter (fun def ->
      // Needs to be the first one, label making is order-dependent
      do def.comps.Insert(0, CompProperties(typeof<CompInfusion>))

      if def.inspectorTabs.NullOrEmpty() then
        do def.inspectorTabs <- ResizeArray<Type>(1)
        do def.inspectorTabsResolved <- ResizeArray<InspectTabBase>(1)

      do def.inspectorTabs.Add(iTabType)
      do def.inspectorTabsResolved.Add(iTab))

  member private this.InjectToStats() =
    //   Many stats are now cached at StatWorker level, and some of them with certain conditions met
    // (e.g. No specific StatWorker, no StatParts) are considered immutable with their cache never expiring.
    //   In other words, adding a StatPart causes immutability check to break which may cause minor
    // performance degradation. So we only add our StatPart to stats which have related infusions.
    do
      DefDatabase<InfusionDef>.AllDefs
      |> Seq.collect (fun def -> def.stats.Keys)
      |> Seq.distinct
      |> Seq.iter (fun def ->
        let statPart = StatPart.Infusion(def)

        if (isNull def.parts) then
          do def.parts <- ResizeArray<StatPart>(1)

        do def.parts.Add statPart)

    // And we have to manually update the caches
    do StatDef.SetImmutability()
