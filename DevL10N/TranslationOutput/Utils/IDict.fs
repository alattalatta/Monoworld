module DevL10N.TranslationOutput.Utils.IDict

open System.Collections.Generic


let get (key: 'a) (d: IDictionary<'a, 'b>) : 'b option =
  let mutable value: 'b = null

  if d.TryGetValue(key, &value) then
    Some value
  else
    None
