module ANDH.ResourceBank

open Verse

open Poet.Lyric.Translation


module Defs =
  let mutable ANDH_MetalhorrorImplant_FalseAlarm: HediffDef = null


module Strings =
  let nuzzleDetectedDesc initiator recipient noticed =
    translate3 "ANDH.NuzzleDetectedDesc" initiator recipient noticed
    
  let nuzzleDetectedDescFallible initiator recipient noticed =
    translate3 "ANDH.NuzzleDetectedDescFallible" initiator recipient noticed

  let nuzzleDetectedBondedDesc initiator recipient noticed =
    translate3 "ANDH.NuzzleBondedDetectedDesc" initiator recipient noticed

  let nuzzleDetectedBondedDescFallible initiator recipient noticed =
    translate3 "ANDH.NuzzleBondedDetectedDescFallible" initiator recipient noticed

  let metalhorrorExplainer initiator recipient =
    translate2 "MetalhorrorNoticedDetailsAppended" initiator recipient

  let metalhorrorReasonNuzzled initiator recipient noticed =
    translate3 "ANDH.MetalhorrorReasonNuzzled" initiator recipient noticed
