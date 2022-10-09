module DevL10N.Lib

open System

open Verse


type SysList<'a> = Collections.Generic.List<'a>

let headTail l =
  match l with
  | [] -> raise (ArgumentOutOfRangeException("l"))
  | h :: t -> h, t

let splitFlipped (list: 'a list) = Poet.Lib.flip List.splitAt list

let taggify (prefix: string) key value =
  if prefix.NullOrEmpty() then
    sprintf "<%s>%s</%s>" key value key
  else
    sprintf "<%s_%s>%s</%s_%s>" prefix key value prefix key
