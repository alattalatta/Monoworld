![INFUSION II](preview.png)

## Development

The assembly source code is located inside `src` directory.
XML documents such as `InfusionDefs` and `LanguageData` are in `xml` directory.

_Infusion 2_ requires `dotnet` CLI, and targets .NET 4.7.2. It's using [Paket](https://fsprojects.github.io/Paket/) for dependency management.

Before developing, create a new directory named `libs` to the project root, and copy following assemblies from RimWorld:
- `Assembly-CSharp-firstpass.dll`
- `Assembly-CSharp.dll`
- `NAudio.dll`
- `UnityEngine.CoreModule.dll`
- `UnityEngine.IMGUIModule.dll`

### Dist building
```
> cd src
> dotnet build
```

This will create `dist` directory for both FSharp.Core and Infusion, populated with their assemblies, XML documents, and other assets.
