module Infusion.Mod

open System
open System.Reflection

open HarmonyLib
open RimWorld
open Verse

open Lib
open VerseTools

let hasQualityNoInfusion (def: ThingDef) =
    def.HasComp(typedefof<CompQuality>) && not (def.HasComp(typedefof<Comp.Infusion>))

let isSingleUse (def: ThingDef) =
    Option.ofObj def.thingSetMakerTags
    |> Option.map (Seq.contains "SingleUseWeapon")
    |> Option.defaultValue false

type ModBase() =
    inherit HugsLib.ModBase()

    override this.ModIdentifier = "latta.infusion"

    override this.DefsLoaded() =
        do Settings.initialize()
        do this.Inject()

    member private this.Inject() =
        this.InjectToThings()
        this.InjectToStats()

    member private this.InjectToThings() =
        let infusionCandidates =
            DefDatabase<ThingDef>.AllDefs
            |> Seq.filter (apparelOrWeapon <&> hasQualityNoInfusion <&> (isSingleUse >> not))

        let iTabType = typedefof<ITab.Infused>
        let iTab = InspectTabManager.GetSharedInstance(iTabType)

        infusionCandidates
        |> Seq.iter (fun def ->
            // Needs to be the first, label making is order-dependent
            do def.comps.Insert(0, CompProperties(typeof<Comp.Infusion>))
            if def.inspectorTabs.NullOrEmpty() then
                do def.inspectorTabs <- ResizeArray<Type>(1)
                do def.inspectorTabsResolved <- ResizeArray<InspectTabBase>(1)

            do def.inspectorTabs.Add(iTabType)
            do def.inspectorTabsResolved.Add(iTab))

    member private this.InjectToStats() =
        do DefDatabase<StatDef>.AllDefs
           |> Seq.filter (fun def -> not def.forInformationOnly)
           |> Seq.iter (fun def ->
               let statPart = StatPart.Infusion(def)

               if (isNull def.parts) then do def.parts <- ResizeArray<StatPart>(1)

               do def.parts.Add statPart)

type Mod(content) =
    inherit Verse.Mod(content)
    do Harmony("com.alattalatta.infusion").PatchAll(Assembly.GetExecutingAssembly())
