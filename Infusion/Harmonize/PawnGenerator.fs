module Infusion.Harmonize.PawnGenerator

open HarmonyLib
open Poet.Lib
open Poet.Lyric
open RimWorld
open Verse

open Infusion


let private handleGear (a: ThingWithComps) =
  let compInfusion = Comp.ofThing<CompInfusion> a

  let compBiocodable =
    Comp.ofThing<CompBiocodable> a
    |> Option.filter (fun cb -> cb.Biocodable && cb.Biocoded)

  Option.map2
    (fun (ci: CompInfusion) (cb: CompBiocodable) ->
      let qualityInt = byte (ci.Quality) + 2uy

      // clamp!
      let quality =
        if qualityInt > byte (QualityCategory.Legendary) then
          QualityCategory.Legendary
        else
          LanguagePrimitives.EnumOfValue qualityInt

      ci.Biocoder <- Some cb
      ci.SlotCount <- ci.CalculateSlotCountFor quality
      ci.SetInfusions(CompInfusion.pickInfusions quality ci, false))
    compInfusion
    compBiocodable
  |> ignore


[<HarmonyPatch(typeof<PawnGenerator>, "GenerateGearFor")>]
// postprocess biocoded equipments
module GenerateGearFor =
  let Postfix (pawn: Pawn, request: PawnGenerationRequest) =
    if Settings.BiocodeBonus.handle.Value then
      Option.ofObj pawn.apparel
      |> Option.map (fun at -> seq at.WornApparel)
      |> Option.defaultValue Seq.empty
      |> Seq.iter (handleGear)

      Option.ofObj pawn.equipment
      |> Option.bind (fun eqt -> Option.ofObj eqt.Primary)
      |> Option.iter handleGear
