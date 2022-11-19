module DevL10N.TranslationOutput.Utils.Seq


let collecti (fn: (int * 'a) -> 'b seq) (s: 'a seq) = s |> Seq.indexed |> Seq.collect fn
