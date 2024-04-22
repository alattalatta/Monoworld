module Poet.Lib

open System
open System.Collections.Generic

open Verse


let err format = Printf.ksprintf Log.Error format
let log format = Printf.ksprintf Log.Message format
let warn format = Printf.ksprintf Log.Warning format

let (<&>) f g = (fun x -> f x && g x)

let feq (a: float32) (b: float32) = (abs (a - b)) < 0.01f

let feq0 = feq 0.0f

let fneq a b = not (feq a b)

let fneq0 = fneq 0.0f

let dictseq (dict: Dictionary<_, _>) =
  seq {
    for entry in dict do
      yield entry
  }

let flip f a b = f b a

let shuffle seq =
  let array = Seq.toArray seq
  let random = Random()

  for i in 0 .. array.Length - 1 do
    let j = random.Next(i, array.Length)
    let pom = array.[i]
    array.[i] <- array.[j]
    array.[j] <- pom

  array |> Array.toSeq

let tap fn a =
  do fn a
  a

let tryCast<'a> (o: obj) =
  match o with
  | :? 'a as o' -> Some o'
  | _ -> None


module Dict =
  let get (key: 'a) (d: IDictionary<'a, 'b>) : 'b option =
    let mutable value: 'b = null

    if d.TryGetValue(key, &value) then
      Some value
    else
      None


module Option =
  let ofResult a =
    match a with
    | Ok a -> Some a
    | Error _ -> None

  let ofSeq s =
    if Seq.isEmpty s then
      None
    else
      Some s

  let tap f a =
    match a with
    | Some a2 ->
      do f (a2)
      a
    | None -> None

  let tapNone f a =
    match a with
    | Some _ -> a
    | None ->
      do f ()
      a


module Result =
  let ofSeq e s =
    if Seq.isEmpty s then
      Error e
    else
      Ok s

  let tapError f =
    Result.mapError
      (fun a ->
        do f (a)
        a)

  let iterOk f r =
    match r with
    | Ok a -> do f a
    | _ -> ()

  let iterBoth fe fa r =
    match r with
    | Ok a -> do fa a
    | Error e -> do fe e
