# alattalatta/Monoworld

- Dev In Your Language (DevL10N)
- Infusion 2
- Poet (FSharp.Core)

---

All projects are located under the `src/`.

Each project consists of following items:

- `assets/`: All non-code assets such as PSD for textures and preview images.
- `xml/`: The mod data files, from Def files to `About.xml`.
- Assembly sources.

Building projects requires .NET CLI (`dotnet`), and we are using [Paket](https://fsprojects.github.io/Paket/) for dependency management. Just use `dotnet tools restore` to install Paket.

## RimWorld assemblies

For some unknown reason, `Krafs.Rimworld.Ref` package doesn't work, so you'll have to manually copy required RimWorld assemblies.

Copy these assemblies to `libs/`:

- `Assembly-CSharp-firstpass.dll`
- `Assembly-CSharp.dll`
- `UnityEngine.CoreModule.dll`
- `UnityEngine.dll`
- `UnityEngine.IMGUIModule.dll`
- `UnityEngine.TextCoreModule.dll`
- `UnityEngine.TextRenderingModule.dll`
- `UnityEngine.UIModule.dll`

## Building mods

To build, simply type:

```
> dotnet build
```

This creates `dist/` containing all the mods, ready to go.

## Work items for better DX

- Only build one project
- Automatically zip the built mods
