module DevL10N.TranslationOutput.Utils.Seq


let choosi (fn: (int * 'a) -> 'b option) (s: 'a seq) = s |> Seq.indexed |> Seq.choose fn
let collecti (fn: (int * 'a) -> 'b seq) (s: 'a seq) = s |> Seq.indexed |> Seq.collect fn
