![INFUSION II](preview.png)

## Development

The assembly source code is located inside `src` directory.
XML documents such as `InfusionDefs` and `LanguageData` are in `xml` directory.

_Infusion 2_ requires `dotnet` CLI, and targets .NET 4.7.2. It's using [Paket](https://fsprojects.github.io/Paket/) for dependency management.

### `InfusionDef`

### Assembly building
```
> cd src
> dotnet build
```

This will create `dist` directory for both FSharp.Core and Infusion, populated with their assembly and XML documents.

[todo] make it runnable at project root