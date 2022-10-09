module DevL10N.TranslationOutput.Utils.Seq


let collecti (fn: (int * 'a) -> 'b seq) (s: 'a seq) = s |> Seq.indexed |> Seq.collect fn

let existsi (predicate: (int * 'a -> bool)) (s: 'a seq) =
  s |> Seq.indexed |> Seq.exists predicate

let ofType<'a, 'b> (items: 'a seq) =
  items
  |> Seq.cast<obj>
  |> Seq.filter (fun x -> x :? 'b)
  |> Seq.cast<'b>
