module ANDH.Mod


type ModBase() =
  inherit HugsLib.ModBase()

  override this.ModIdentifier = "latta.andh"

  override this.DefsLoaded() = Settings.initialize ()
