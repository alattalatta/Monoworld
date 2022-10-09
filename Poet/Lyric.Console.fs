module Poet.Lyric.Console

open Verse


let log format = Printf.ksprintf Log.Message format
