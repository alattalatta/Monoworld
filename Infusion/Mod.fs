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
      def.ResolveReferences()
      DefGenerator.AddImpliedDef<ThingDef>(def)

      Option.ofObj def.thingCategories
      |> Option.iter (Seq.iter (fun cat -> cat.childThingDefs.Add def))

      HugsLib.Utils.InjectedDefHasher.GiveShortHashToDef(def, typeof<ThingDef>))

    // Resolve references for filters
    // ThingCategory itself (for stockpiles)
    Option.ofObj (ThingCategoryDef.Named("Infusion_Infusers"))
    |> Option.iter ((fun def -> def.ResolveReferences()))

    // Storable buildings require separate calls
    DefsForReading.allBuildings
    |> Seq.collect (fun def ->
      Option.ofObj def.building
      |> Option.map (fun building ->
        let defFilter = Option.ofObj building.defaultStorageSettings
        let fixedFilter = Option.ofObj building.fixedStorageSettings

        Seq.choose id ([ defFilter; fixedFilter ]))
      |> Option.defaultValue Seq.empty)
    |> Seq.iter (fun storage -> storage.filter.ResolveReferences())


type ModBase() =
  inherit HugsLib.ModBase()

  override this.ModIdentifier = "latta.infusion"

  override this.SceneLoaded _ = CompInfusion.ClearCaches()

  override this.DefsLoaded() =
    Settings.initialize ()
    this.Inject()
    Infusion.Harmonize.StatWorker.populateStatsEligibilityMap ()

  member private this.Inject() =
    this.InjectToThings()
    this.InjectToStats()

  member private this.InjectToThings() =
    let iTabType = typedefof<ITab.Infused>
    let iTab = InspectTabManager.GetSharedInstance(iTabType)

    DefsForReading.allThingsInfusable
    |> List.iter (fun def ->
      // Needs to be the first one, label making is order-dependent
      def.comps.Insert(0, CompProperties(typeof<CompInfusion>))

      if def.inspectorTabs.NullOrEmpty() then
        def.inspectorTabs <- ResizeArray<Type>(1)
        def.inspectorTabsResolved <- ResizeArray<InspectTabBase>(1)

      def.inspectorTabs.Add(iTabType)
      def.inspectorTabsResolved.Add(iTab))

  member private this.InjectToStats() =
    //   Many stats are now cached at StatWorker level, and some of them with certain conditions met
    // (e.g. No specific StatWorker, no StatParts) are considered immutable with their cache never expiring.
    //   In other words, adding a StatPart causes immutability check to break which may cause minor
    // performance degradation. So we only add our StatPart to stats which have related infusions.
    DefDatabase<InfusionDef>.AllDefs
    |> Seq.collect (fun def -> def.stats.Keys)
    |> Seq.distinct
    |> Seq.iter (fun def ->
      let statPart = StatPart.Infusion(def)

      if (isNull def.parts) then
        def.parts <- ResizeArray<StatPart>(1)

      def.parts.Add statPart)

    // And we have to manually update the caches
    StatDef.SetImmutability()
