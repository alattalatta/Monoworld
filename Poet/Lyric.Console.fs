module Poet.Lyric.Console

open Verse


let err format = Printf.ksprintf Log.Error format
let log format = Printf.ksprintf Log.Message format
let warn format = Printf.ksprintf Log.Warning format
