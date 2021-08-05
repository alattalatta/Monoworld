module DevL10N.TranslationOutput.Utils.List


let intersperse sep ls =
  List.foldBack
    (fun x ->
      function
      | [] -> [ x ]
      | xs -> x :: sep :: xs)
    ls
    []