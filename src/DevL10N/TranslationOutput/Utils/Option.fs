module DevL10N.TranslationOutput.Utils.Option


type MaybeBuilder() =
  member _.Bind(x, f) =
    match x with
    | None -> None
    | Some a -> f a

  member _.Return(x) = Some x

  member _.ReturnFrom(x) = x

  member _.Zero() = None

let maybe = MaybeBuilder()