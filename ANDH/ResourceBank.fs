module ANDH.ResourceBank

open Poet.Lyric.Translation


module Strings =
  let nuzzleDetectedDesc initiator recipient noticed =
    translate3 "ANDH.NuzzleDetectedDesc" initiator recipient noticed

  let metalhorrorExplainer initiator recipient =
    translate2 "MetalhorrorNoticedDetailsAppended" initiator recipient

  let metalhorrorReasonNuzzled initiator recipient noticed =
    translate3 "ANDH.MetalhorrorReasonNuzzled" initiator recipient noticed
