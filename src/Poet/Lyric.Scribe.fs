module Poet.Lyric.Scribe

open Verse


/// Scribes a value.
///
/// When saving, returns None.
///
/// When loading, if the saved data exists, returns it in Some. Otherwise returns None.
let value key value =
    let mutable out = value

    match Scribe.mode with
    | LoadSaveMode.LoadingVars ->
        do Scribe_Values.Look(&out, key)
        Some out
    | LoadSaveMode.Saving ->
        do Scribe_Values.Look(&out, key)
        None
    | _ -> None

/// Scribes a value, with the default value.
///
/// When saving, returns None.
///
/// When loading, if the saved data exists, returns it in Some. Otherwise returns None.
let valueDefault key defaultValue value =
    let mutable out = value

    match Scribe.mode with
    | LoadSaveMode.LoadingVars ->
        do Scribe_Values.Look(&out, key, defaultValue)
        Some out
    | LoadSaveMode.Saving ->
        do Scribe_Values.Look(&out, key, defaultValue)
        None
    | _ -> None

/// Scribes a nullable value.
///
/// When saving, returns None.
///
/// When loading, if the saved data exists, returns it in Some. Otherwise returns None.
let valueNullable key value =
    let mutable out = value

    match Scribe.mode with
    | LoadSaveMode.LoadingVars ->
        do Scribe_Values.Look(&out, key)
        Option.ofObj out
    | LoadSaveMode.Saving ->
        do Scribe_Values.Look(&out, key)
        None
    | _ -> None

/// Scribes a sequence of defs.
///
/// When saving, returns None.
///
/// When loading, if the saved data exists, returns it in Some. Otherwise returns None.
let defCollection<'a when 'a :> Def> key (defs: 'a seq) =
    let mutable out = ResizeArray defs

    match Scribe.mode with
    | LoadSaveMode.LoadingVars ->
        do Scribe_Collections.Look(&out, key, LookMode.Def)
        Option.ofObj out
        |> Option.map (fun a -> seq { yield! a })
    | LoadSaveMode.Saving ->
        do Scribe_Collections.Look(&out, key, LookMode.Def)
        None
    | _ -> None
