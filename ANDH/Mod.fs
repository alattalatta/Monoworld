module ANDH.Mod

open Verse


type ModBase() =
  inherit HugsLib.ModBase()

  override this.ModIdentifier = "latta.andh"

  override this.DefsLoaded() =
    Settings.initialize ()
    ResourceBank.Defs.ANDH_MetalhorrorImplant_FalseAlarm <- HediffDef.Named("ANDH_MetalhorrorImplant_FalseAlarm")
