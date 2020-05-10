module Infusion.ITab

open System.Text

open RimWorld
open UnityEngine
open Verse
open Verse.Sound

open VerseInterop
open VerseTools

let trashTex =
    ContentFinder<Texture2D>.Get("UI/Infusion_Trash")

type Infused() =
    inherit ITab(size = Vector2(400.0f, 550.0f), labelKey = "Infusion.ITab")

    let mutable scrollPos = Vector2.zero

    // All drawing methods assume this to be Some,
    // as IsVisible only passes when there is CompInfusion.
    member this.CompInf =
        match Option.ofObj this.SelThing with
        | Some thing -> compOfThing<Comp.Infusion> thing
        | None -> None

    member this.DrawLabel(parentRect: Rect) =
        let compInf = this.CompInf.Value

        let label = compInf.InspectionLabel

        do Text.Font <- GameFont.Medium
        do GUI.color <-
            match compInf.BestInfusion with
            | Some inf -> tierToColor inf.tier
            | None -> Color.white
        do Widgets.Label(parentRect, label)

        Text.CalcHeight(label, parentRect.width)

    member this.DrawSubLabel(parentRect: Rect) =
        let compInf = this.CompInf.Value
        let compQuality = compOfThing<CompQuality> this.SelThing

        let subLabelSB = StringBuilder()

        if Option.isSome compQuality then
            do subLabelSB.Append(compQuality.Value.Quality.GetLabel()).Append(" ")
               |> ignore

        if not (isNull compInf.parent.Stuff) then
            do subLabelSB.Append(compInf.parent.Stuff.LabelAsStuff).Append(" ")
               |> ignore

        let subLabel =
            (string (subLabelSB.Append(compInf.parent.def.label))).CapitalizeFirst()

        do Text.Font <- GameFont.Small
        do GUI.color <- Color.white
        do Widgets.Label(parentRect, subLabel)

        Text.CalcHeight(subLabel, parentRect.width)

    member this.DrawInfusion (parentRect: Rect) yOffset (infDef: InfusionDef) =
        let compInf = this.CompInf.Value // assumes this.Comp to be always Some

        let description = infDef.GetDescriptionString()

        let contentsHeight =
            Text.CalcHeight(description, parentRect.width)
            + 16.0f

        let container =
            Rect(parentRect.x, yOffset, parentRect.width, contentsHeight)

        let body =
            Rect(container.x, container.y + 8.0f, container.xMax, container.yMax - 8.0f)

        let hovered = Mouse.IsOver container

        // removal highlight
        let markedForRemoval = Set.contains infDef compInf.RemovalSet
        if markedForRemoval then
            do GUI.color <- Color(1.0f, 0.0f, 0.0f, 0.85f)
            do GUI.DrawTexture(container, TexUI.HighlightTex)

        // hover highlight
        if hovered
        then do GUI.DrawTexture(container, TexUI.HighlightTex)

        let tooltipStringKey =
            if markedForRemoval then "Infusion.ITab.UnmarkForRemoval" else "Infusion.ITab.MarkForRemoval"

        TooltipHandler.TipRegion(container, TipSignal(translate tooltipStringKey))

        if Widgets.ButtonInvisible(container) then
            if markedForRemoval then
                do compInf.UnmarkForRemoval infDef
                do SoundDefOf.Checkbox_TurnedOn.PlayOneShotOnCamera()
            else
                do compInf.MarkForRemoval infDef
                do SoundDefOf.Checkbox_TurnedOff.PlayOneShotOnCamera()

        do GUI.color <- Color.white
        do Widgets.Label(body, description)

        contentsHeight

    member this.DrawInfusionList(parentRect: Rect) =
        let compInf = this.CompInf.Value

        let scrollerWidth = parentRect.width - 20.0f

        let totalHeight =
            compInf.Infusions
            |> Seq.fold (fun acc cur ->
                acc
                + Text.CalcHeight(cur.GetDescriptionString(), scrollerWidth)
                + 24.0f) 0.0f

        let scroller =
            Rect(0.0f, 0.0f, scrollerWidth, totalHeight)

        do Widgets.BeginScrollView(parentRect, &scrollPos, scroller)

        do compInf.Infusions
           |> Seq.fold (fun acc cur ->
               acc
               + this.DrawInfusion scroller (scroller.y + acc) cur
               + 8.0f) 0.0f
           |> ignore

        do Widgets.EndScrollView()

    override this.IsVisible =
        match this.CompInf with
        | Some compInf -> compInf.Size > 0
        | None -> false

    override this.FillTab() =
        let container =
            Rect(0.0f, 0.0f, this.size.x, this.size.y).ContractedBy(16.0f)

        // label
        let labelView =
            Rect(container.xMin, container.yMin, container.xMax * 0.85f, container.yMax)

        let labelHeight = this.DrawLabel(labelView) + 4.0f

        // subLabel
        let subLabelView =
            Rect(container.xMin, labelView.yMin + labelHeight, container.width, container.yMax - labelHeight)

        let subLabelHeight = this.DrawSubLabel(subLabelView) + 12.0f

        // infusions
        let descView =
            Rect
                (container.xMin,
                 subLabelView.yMin + subLabelHeight,
                 container.xMax - 6.0f,
                 container.yMax
                 - subLabelView.yMin
                 - subLabelHeight)

        do this.DrawInfusionList(descView)
