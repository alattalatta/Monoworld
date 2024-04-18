module Infusion.Lib

open FSharp.Collections


let rec runUntilFalseFrom (index: int) (fn: 'a -> bool, s: 'a seq) =
  let arr = match s with
            | :? ResizeArray<'a> as a -> a
            | x -> new ResizeArray<'a>(x)
    
  if index = arr.Count then
    true
  else
    let worker = arr[index]
    let result = fn worker
    
    if result then
      runUntilFalseFrom (index + 1) (fn, arr)
    else
      false
