module Infusion.DefsForReading

open Poet.Lib
open RimWorld
open Verse


/// Predicate for determining whether given ThingDef is an apparel, a weapon, or not.
let private apparelOrWeapon (def: ThingDef) =
  ThingCategoryDefOf.Apparel.ContainedInThisOrDescendant def
  || ThingCategoryDefOf.Weapons.ContainedInThisOrDescendant def

let private hasQualityNoInfusion (def: ThingDef) =
  def.HasComp(typedefof<CompQuality>)
  && not (def.HasComp(typedefof<CompInfusion>))

let private isMultiUse (def: ThingDef) =
  Option.ofObj def.thingSetMakerTags
  |> Option.map (not << Seq.contains "SingleUseWeapon")
  |> Option.defaultValue true

let allThingsInfusable =
  DefDatabase<ThingDef>.AllDefs
  |> Seq.filter (
    apparelOrWeapon
    <&> hasQualityNoInfusion
    <&> isMultiUse
  )
  |> Seq.map (tap (fun x -> Poet.Lyric.Console.log "%A" x.defName))
  |> List.ofSeq
