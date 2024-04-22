module Poet.Lyric.Translation

open Verse


let private toNamedArg (a: obj) =
  match a with
  | :? NamedArgument as na -> na
  | _ -> NamedArgument(a, null)


let translate (key: string) = key.TranslateSimple()

let translate1 (key: string) a =
  key.Translate (toNamedArg a) |> string

let translate2 (key: string) a b =
  key.Translate (toNamedArg a, toNamedArg b)
  |> string

let translate3 (key: string) a b c =
  key.Translate (toNamedArg a, toNamedArg b, toNamedArg c)
  |> string
 