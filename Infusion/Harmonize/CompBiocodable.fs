module Infusion.Harmonize.CompBiocodable

open HarmonyLib
open Poet.Lyric
open RimWorld
open Verse

open Infusion
open VerseTools


[<HarmonyPatch(typeof<CompBiocodable>, "CodeFor")>]
// separate flow for infusing biocodeds
module CodeFor =
  let Prefix (__instance: CompBiocodable, p: Pawn) =
    // Exclude bladelink: It may cause infusions to change when a colonist equips a fresh one
    if (not (__instance :? CompBladelinkWeapon)) && Settings.BiocodeBonus.handle.Value then
      Comp.ofThing<CompInfusion> __instance.parent
      |> Option.iter (fun comp ->
        let qualityInt = byte (comp.Quality) + 2uy

        // clamp!
        let quality =
          if qualityInt > byte (QualityCategory.Legendary) then
            QualityCategory.Legendary
          else
            LanguagePrimitives.EnumOfValue qualityInt

        comp.Biocoder <- Some __instance
        comp.SlotCount <- comp.CalculateSlotCountFor quality
        comp.SetInfusions(CompInfusion.pickInfusions quality comp, false))

  let Postfix (__instance: CompQuality) =
    // See Harmonize.CompQuality.SetQuality:Postfix
    Comp.getParent __instance |> resetHP
