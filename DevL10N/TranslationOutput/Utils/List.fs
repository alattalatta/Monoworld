module DevL10N.TranslationOutput.Utils.List


let collecti (fn: (int * 'a) -> 'b list) (s: 'a list) = s |> List.indexed |> List.collect fn

let intersperse sep ls =
  List.foldBack
    (fun x ->
      function
      | [] -> [ x ]
      | xs -> x :: sep :: xs)
    ls
    []