module DevL10N.Lib

open System
open System.Reflection

open Verse


type SysList<'a> = Collections.Generic.List<'a>

// move to Poet?
let add a b = a + b

let headTail l =
    match l with
    | [] -> raise (ArgumentOutOfRangeException("l"))
    | h :: t -> h, t

let splitFlipped (list: 'a list) = Poet.Lib.flip List.splitAt list

let taggify prefix key value =
    sprintf "<%s_%s>%s</%s_%s>" prefix key value prefix key
