![INFUSION II](preview.png)

## Development

The assembly source code is located inside `src` directory.
XML documents such as `InfusionDefs` and `LanguageData` are in `xml` directory.

_Infusion 2_ requires `dotnet` CLI, and targets .NET 4.7.2. It's using [Paket](https://fsprojects.github.io/Paket/) for dependency management.

### Overview
`InfusionDef` represents an infusion's XML definition. `DefFields` are complex, "non-primitive" types for `InfusionDef`s.

`Comp` holds an item's infusions (as `InfusionDef`s), and provides functions / methods for itself and its parent Thing.
`StatMod` represents stat modifiers for infusions.
`StatPart` and `Harmonize/StatWorker` use `Comp` to apply `StatMod` to a Thing's stat, and a Pawn's stat who equips the Thing, respectively.

`Mod` is _Infusion 2_'s entry point. This is where `Comp` is injected into all apparels and weapons. It also initializes `Settings`.

`DefTool`, `Lib`, and `VerseInterop` are sets of utility functions.

### Dist building
```
> cd src
> dotnet build
```

This will create `dist` directory for both FSharp.Core and Infusion, populated with their assembly, XML documents, and other assets.
