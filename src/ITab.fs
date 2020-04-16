module Infusion.ITab

open System.Text

open RimWorld
open UnityEngine
open Verse

open DefTool
open VerseInterop

type Infused() =
    inherit ITab(size = Vector2(400.0f, 550.0f), labelKey = "Infusion.ITab")

    member this.CompInf =
        match Option.ofObj this.SelThing with
        | Some thing -> compOfThing<Comp.Infusion> thing
        | None -> None

    override this.IsVisible =
        match this.CompInf with
        | Some compInf -> compInf.Size > 0
        | None -> false

    override this.FillTab() =
        let thing = this.SelThing
        let compInf = this.CompInf.Value // assumes this.Comp to be always Some
        let compQuality = compOfThing<CompQuality> thing

        let container = Rect(0.0f, 0.0f, this.size.x, this.size.y).ContractedBy(16.0f)

        // label
        let rectLabel = Rect(container.xMin, container.yMin, container.xMax * 0.85f, container.yMax)
        Text.Font <- GameFont.Medium
        GUI.color <-
            match compInf.BestInfusion with
            | Some inf -> tierToColor inf.tier
            | None -> Color.white

        let label = compInf.LabelFull
        Widgets.Label(rectLabel, label)

        // subLabel
        let rectQuality =
            Rect
                (container.xMin, rectLabel.yMin + Text.CalcHeight(label, rectLabel.width) + 4.0f, container.xMax,
                 container.yMax)
        Text.Font <- GameFont.Small
        GUI.color <- Color.white

        let subLabelSB = StringBuilder()

        if Option.isSome compQuality then
            do subLabelSB.Append(compQuality.Value.Quality.GetLabel()).Append(" ") |> ignore

        if not (isNull compInf.parent.Stuff) then
            do subLabelSB.Append(compInf.parent.Stuff.LabelAsStuff).Append(" ") |> ignore

        let subLabel = (string (subLabelSB.Append(compInf.parent.def.label))).CapitalizeFirst()
        Widgets.Label(rectQuality, subLabel)

        // desciptions
        let rectDesc =
            Rect
                (container.xMin, rectQuality.yMin + Text.CalcHeight(subLabel, this.size.x) + 12.0f, container.xMax,
                 container.yMax)
        GUI.color <- Color.white

        Widgets.Label(rectDesc, compInf.Descriptions)
