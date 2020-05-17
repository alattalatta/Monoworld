module Infusion.ITab

open System.Text

open RimWorld
open UnityEngine
open Verse
open Verse.Sound

open Lib
open VerseInterop

let gray = Color(0.61f, 0.61f, 0.61f)

type Infused() =
    inherit ITab(size = Vector2(400.0f, 550.0f), labelKey = "Infusion.ITab")

    let mutable scrollPos = Vector2.zero

    // All drawing methods assume this to be Some,
    // as IsVisible only passes when there is CompInfusion.
    member this.CompInf =
        match Option.ofObj this.SelThing with
        | Some thing -> compOfThing<CompInfusion> thing
        | None -> None

    member this.DrawLabel(parentRect: Rect) =
        let compInf = this.CompInf.Value

        let label = compInf.InspectionLabel

        do Text.Font <- GameFont.Medium
        do GUI.color <-
            match compInf.BestInfusion with
            | Some inf -> inf.tier.color
            | None -> gray
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

        do subLabelSB.Append(compInf.parent.def.label).Append(" (").Append(compInf.Size).Append("/")
                     .Append(compInf.SlotCount).Append(")")
           |> ignore

        let subLabel = (string subLabelSB).CapitalizeFirst()

        do Text.Font <- GameFont.Small
        do GUI.color <- Color.white
        do Widgets.Label(parentRect, subLabel)

        Text.CalcHeight(subLabel, parentRect.width)

    member this.DrawBaseInfusion (parentRect: Rect) yOffset (infDef: InfusionDef) =
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

        // hover highlight
        if hovered
        then do GUI.DrawTexture(container, TexUI.HighlightTex)

        do GUI.color <- Color.white
        do Widgets.Label(body, description)

        container

    member this.DrawInfusion (parentRect: Rect) yOffset (infDef: InfusionDef) =
        let compInf = this.CompInf.Value // assumes this.Comp to be always Some

        let container =
            this.DrawBaseInfusion parentRect yOffset infDef

        // extraction/removal highlight
        let markedForExtraction =
            Set.contains infDef compInf.ExtractionSet

        let markedForRemoval = Set.contains infDef compInf.RemovalSet

        if markedForExtraction then
            do GUI.color <- Color(1.0f, 1.0f, 0.0f, 0.85f)
            do GUI.DrawTexture(container, TexUI.HighlightTex)
        elif markedForRemoval then
            do GUI.color <- Color(1.0f, 0.0f, 0.0f, 0.85f)
            do GUI.DrawTexture(container, TexUI.HighlightTex)

        do GUI.color <- Color.white

        let tooltipStringKey =
            if markedForExtraction
            then translate "Infusion.ITab.MarkForRemoval"
            elif markedForRemoval
            then translate "Infusion.ITab.Unmark"
            else string (translate1 "Infusion.ITab.MarkForExtraction" (infDef.tier.extractionChance.ToStringPercent()))

        do TooltipHandler.TipRegion(container, TipSignal(tooltipStringKey))

        if Widgets.ButtonInvisible(container) then
            if markedForExtraction then
                do compInf.MarkForRemoval infDef
                do SoundDefOf.Checkbox_TurnedOn.PlayOneShotOnCamera()
            elif markedForRemoval then
                do compInf.UnmarkForRemoval infDef
                do SoundDefOf.Checkbox_TurnedOff.PlayOneShotOnCamera()
            else
                do compInf.MarkForExtractor infDef
                do SoundDefOf.Checkbox_TurnedOn.PlayOneShotOnCamera()

        container

    member this.DrawPendingInfusion (parentRect: Rect) yOffset (infDef: InfusionDef) =
        let compInf = this.CompInf.Value // assumes this.Comp to be always Some

        let container =
            this.DrawBaseInfusion parentRect yOffset infDef

        // pending highlight
        do GUI.color <- Color(0.0f, 1.0f, 0.0f, 0.85f)
        do GUI.DrawTexture(container, TexUI.HighlightTex)

        let tooltipStringKey = "Infusion.ITab.CancelInfuser"

        do TooltipHandler.TipRegion(container, TipSignal(translate tooltipStringKey))

        if Widgets.ButtonInvisible(container) then
            do compInf.UnmarkForInfuser infDef
            do SoundDefOf.Checkbox_TurnedOn.PlayOneShotOnCamera()

        container

    member this.DrawInfusionList(parentRect: Rect) =
        let comp = this.CompInf.Value

        let scrollerWidth = parentRect.width - 20.0f

        let calculateInfusionsHeight (infusions: InfusionDef seq) =
            infusions
            |> Seq.fold (fun acc cur ->
                acc
                + Text.CalcHeight(cur.GetDescriptionString(), scrollerWidth)
                + 24.0f) 0.0f

        let totalHeight =
            calculateInfusionsHeight comp.Infusions
            + calculateInfusionsHeight comp.WantingSet

        let scroller =
            Rect(0.0f, 0.0f, scrollerWidth, totalHeight)

        do Widgets.BeginScrollView(parentRect, &scrollPos, scroller)

        let infusionsHeight =
            comp.Infusions
            |> Seq.fold (fun acc cur ->
                acc
                + (this.DrawInfusion scroller (scroller.y + acc) cur).height
                + 8.0f) 0.0f

        do comp.WantingSet
           |> Seq.fold (fun acc cur ->
               acc
               + (this.DrawPendingInfusion scroller (scroller.y + acc) cur).height) infusionsHeight
           |> ignore

        do Widgets.EndScrollView()

        do GUI.color <- Color.white

    override this.IsVisible =
        match this.CompInf with
        | Some compInf -> compInf.SlotCount > 0 || compInf.Size > 0
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
                 - subLabelHeight
                 - 40.0f)

        do this.DrawInfusionList(descView)

        // infuser button
        let infuserView =
            Rect(Vector2(container.center.x - 60.0f, container.yMax - 32.0f), Vector2(120.0f, 24.0f))

        let comp = this.CompInf.Value

        if Set.count comp.InfusionsRaw < comp.SlotCount then
            Infuser.AllInfusersByDef
            |> Seq.filter (fun kv ->
                not (Set.contains kv.Key comp.InfusionsRaw)
                && CompInfusion.compatibleWith comp kv.Key)
            |> Option.ofSeq
            |> Option.filter (fun _ ->
                do TooltipHandler.TipRegion(infuserView, TipSignal(translate "Infusion.ITab.ApplyInfuser.Description"))
                Widgets.ButtonText(infuserView, translate "Infusion.ITab.ApplyInfuser"))
            |> Option.iter (fun eligibles ->
                eligibles
                |> Seq.map (fun a ->
                    FloatMenuOption(string (a.Key.LabelCap), (fun () -> do comp.MarkForInfuser a.Key |> ignore)))
                |> System.Collections.Generic.List<FloatMenuOption>
                |> FloatMenu
                |> Find.WindowStack.Add)
