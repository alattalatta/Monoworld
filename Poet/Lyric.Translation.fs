module Poet.Lyric.Translation

open Verse


let translate (key: string) = key.TranslateSimple()

let translate1 (key: string) a =
  key.Translate(NamedArgument(a, null)) |> string

let translate2 (key: string) a b =
  key.Translate(NamedArgument(a, null), NamedArgument(b, null))
  |> string
