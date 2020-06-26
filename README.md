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

Building projects requires .NET CLI (`dotnet`), and we are using [Paket](https://fsprojects.github.io/Paket/) for dependency management.

To build, simply type the following (after installing necessities):

```
> dotnet build
```

This will create `dist/` containing all the mods, ready to go.

## Work items for better DX
- Only build one project
- Automatically zip the built mods
