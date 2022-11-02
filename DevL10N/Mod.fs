namespace DevL10N

open HarmonyLib
open Verse


[<StaticConstructorOnStartup>]
type StartupConstructor() =
  static do
    let harmony = Harmony("latta.devl10n")
    do harmony.PatchAll()
