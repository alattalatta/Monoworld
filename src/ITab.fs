module Infusion.ITab

open System.Text

open RimWorld
open UnityEngine
open Verse

open VerseInterop
open VerseTools

type Infused() =
    inherit ITab(size = Vector2(400.0f, 550.0f), labelKey = "Infusion.ITab")

    let mutable scrollPos = Vector2.zero

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
        let label = compInf.InspectionLabel

        Text.Font <- GameFont.Medium
        GUI.color <-
            match compInf.BestInfusion with
            | Some inf -> tierToColor inf.tier
            | None -> Color.white
        Widgets.Label(rectLabel, label)

        // subLabel
        let labelArea = Text.CalcHeight(label, rectLabel.width) + 4.0f
        let rectQuality = Rect(container.xMin, rectLabel.yMin + labelArea, container.width, container.yMax - labelArea)

        let subLabelSB = StringBuilder()

        if Option.isSome compQuality then
            do subLabelSB.Append(compQuality.Value.Quality.GetLabel()).Append(" ") |> ignore

        if not (isNull compInf.parent.Stuff) then
            do subLabelSB.Append(compInf.parent.Stuff.LabelAsStuff).Append(" ") |> ignore

        let subLabel = (string (subLabelSB.Append(compInf.parent.def.label))).CapitalizeFirst()

        Text.Font <- GameFont.Small
        GUI.color <- Color.white
        Widgets.Label(rectQuality, subLabel)

        // desciptions
        let subLabelArea = Text.CalcHeight(subLabel, container.width) + 12.0f
        let rectDescView =
            Rect
                (container.xMin, rectQuality.yMin + subLabelArea, container.xMax - 6.0f,
                 container.yMax - rectQuality.yMin - subLabelArea)

        let descriptions = compInf.Descriptions
        let descHeight = Text.CalcHeight(descriptions, rectDescView.width)

        let rectDescScroller = Rect(0.0f, 0.0f, rectDescView.width - 20.0f, descHeight)

        Widgets.BeginScrollView(rectDescView, &scrollPos, rectDescScroller)

        GUI.color <- Color.white
        Widgets.Label(rectDescScroller, compInf.Descriptions)

        Widgets.EndScrollView()
