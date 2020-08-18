module Infusion.ITab

open System.Text

open Poet.Lib
open Poet.Lyric
open RimWorld
open UnityEngine
open Verse
open Verse.Sound

open System.Collections.Generic

let gray = Color(0.61f, 0.61f, 0.61f)

let resetStyles () =
    do Text.Font <- GameFont.Small
    do GUI.color <- Color.white

type Infused() =
    inherit ITab(size = Vector2(460.0f, 550.0f), labelKey = "Infusion.ITab")

    let mutable scrollPos = Vector2.zero

    // All drawing methods assume this to be Some,
    // as IsVisible only passes when there is CompInfusion.
    member this.CompInf =
        match Option.ofObj this.SelThing with
        | Some thing -> Comp.ofThing<CompInfusion> thing
        | None -> None

    member private this.DrawLabel(parentRect: Rect) =
        let compInf = this.CompInf.Value

        let label = compInf.InspectionLabel

        do Text.Font <- GameFont.Medium
        do GUI.color <-
            match compInf.BestInfusion with
            | Some inf -> inf.tier.color
            | None -> gray
        do Widgets.Label(parentRect, label)

        let labelHeight = Text.CalcHeight(label, parentRect.width)
        do resetStyles ()

        labelHeight

    member private this.DrawSubLabel(parentRect: Rect) =
        let compInf = this.CompInf.Value
        let compQuality = Comp.ofThing<CompQuality> this.SelThing

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
        do Widgets.Label(parentRect, subLabel)

        let subLabelHeight =
            Text.CalcHeight(subLabel, parentRect.width)

        do Text.Font <- GameFont.Tiny
        do GUI.color <- Color.gray

        let hint = ResourceBank.Strings.ITab.hint
        let hintHeight = Text.CalcHeight(hint, parentRect.width)

        let hintView =
            Rect(parentRect.xMin, parentRect.yMin + subLabelHeight + 4.0f, parentRect.width, hintHeight)

        do Widgets.Label(hintView, hint)
        do resetStyles ()

        subLabelHeight + hintHeight + 4.0f

    member private this.DrawBaseInfusion (parentRect: Rect) yOffset (infDef: InfusionDef) =
        let description = InfusionDef.makeDescriptionString infDef

        let contentsHeight =
            Text.CalcHeight(description, parentRect.width - 16.0f)
            + 16.0f

        let container =
            Rect(parentRect.x, yOffset, parentRect.width, contentsHeight)

        let body =
            Rect(container.x + 8.0f, container.y + 8.0f, container.xMax - 8.0f, container.yMax - 8.0f)

        let hovered = Mouse.IsOver container

        // hover highlight
        if hovered
        then do GUI.DrawTexture(container, TexUI.HighlightTex)

        do Widgets.Label(body, description)

        container

    member private this.DrawInfusion (parentRect: Rect) yOffset (infDef: InfusionDef) =
        let comp = this.CompInf.Value // assumes this.Comp to be always Some

        let container =
            this.DrawBaseInfusion parentRect yOffset infDef

        // extraction/removal highlight
        let markedForExtraction = Set.contains infDef comp.ExtractionSet

        let markedForRemoval = Set.contains infDef comp.RemovalSet

        if markedForExtraction then
            do GUI.color <- Color(1.0f, 1.0f, 0.0f, 0.85f)
            do GUI.DrawTexture(container, TexUI.HighlightTex)
        elif markedForRemoval then
            do GUI.color <- Color(1.0f, 0.0f, 0.0f, 0.85f)
            do GUI.DrawTexture(container, TexUI.HighlightTex)

        do GUI.color <- Color.white

        let baseExtractionChance =
            min
                1.0f
                (infDef.tier.extractionChance
                 * Settings.ExtractionChanceFactor.handle.Value)

        let successChance =
            comp.Biocoder
            |> Option.map (fun _ -> baseExtractionChance * 0.5f)
            |> Option.defaultValue baseExtractionChance
            |> GenText.ToStringPercent

        let tooltipStringKey =
            if markedForExtraction
            then ResourceBank.Strings.ITab.markRemoval successChance
            elif markedForRemoval
            then ResourceBank.Strings.ITab.unmark
            else ResourceBank.Strings.ITab.markExtraction successChance

        do TooltipHandler.TipRegion(container, TipSignal(tooltipStringKey))

        if Widgets.ButtonInvisible(container) then
            if markedForExtraction then
                do comp.MarkForRemoval infDef
                do SoundDefOf.Checkbox_TurnedOn.PlayOneShotOnCamera()
            elif markedForRemoval then
                do comp.UnmarkForRemoval infDef
                do SoundDefOf.Checkbox_TurnedOff.PlayOneShotOnCamera()
            else
                do comp.MarkForExtractor infDef
                do SoundDefOf.Checkbox_TurnedOn.PlayOneShotOnCamera()

        container

    member private this.DrawPendingInfusion (parentRect: Rect) yOffset (infDef: InfusionDef) =
        let compInf = this.CompInf.Value // assumes this.Comp to be always Some

        let container =
            this.DrawBaseInfusion parentRect yOffset infDef

        // pending highlight
        do GUI.color <- Color(0.0f, 1.0f, 0.0f, 0.85f)
        do GUI.DrawTexture(container, TexUI.HighlightTex)

        do TooltipHandler.TipRegion(container, TipSignal(ResourceBank.Strings.ITab.cancelInfuser))

        if Widgets.ButtonInvisible(container) then
            do compInf.UnmarkForInfuser infDef
            do SoundDefOf.Checkbox_TurnedOn.PlayOneShotOnCamera()

        container

    member private this.DrawInfusionList(parentRect: Rect) =
        let comp = this.CompInf.Value

        let scrollerWidth = parentRect.width - 24.0f

        let calculateInfusionsHeight (infusions: InfusionDef seq) =
            infusions
            |> Seq.fold (fun acc cur ->
                acc
                + Text.CalcHeight(InfusionDef.makeDescriptionString cur, scrollerWidth)
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

        do resetStyles ()

    member private this.DrawApplyButton(parentRect: Rect) =
        let comp = this.CompInf.Value

        let drawUnavailableReason (reason: string) =
            do GUI.color <- gray
            do Text.Anchor <- TextAnchor.MiddleCenter
            do Text.Font <- GameFont.Tiny

            do Widgets.Label(parentRect, reason)

            do GUI.color <- Color.white
            do Text.Anchor <- TextAnchor.UpperLeft
            do Text.Font <- GameFont.Small

        let drawButton (infPairs: KeyValuePair<InfusionDef, Infuser> seq) =
            let buttonView =
                Rect(parentRect.xMin + 30.0f, parentRect.yMin, 140.0f, parentRect.height)

            do TooltipHandler.TipRegion(parentRect, TipSignal(ResourceBank.Strings.ITab.applyInfuserDesc))

            if Widgets.ButtonText(buttonView, ResourceBank.Strings.ITab.applyInfuser) then
                do infPairs
                   |> Seq.map (fun infPair ->
                       FloatMenuOption
                           (string (infPair.Key.LabelCap), (fun () -> do comp.MarkForInfuser infPair.Key |> ignore)))
                   |> List<FloatMenuOption>
                   |> FloatMenu
                   |> Find.WindowStack.Add

        let allInfusers =
            if Set.count comp.InfusionsRaw < comp.SlotCount
            then Ok Infuser.AllInfusersByDef
            else Error(ResourceBank.Strings.ITab.cantApplySlotsFull)

        do allInfusers
           |> Result.map
               (Seq.filter (fun kv ->
                   not (Set.contains kv.Key comp.InfusionsRaw)
                   && InfusionDef.checkAllComplexes comp.parent comp.Quality kv.Key))
           |> Result.bind (Result.ofSeq ResourceBank.Strings.ITab.cantApplyNoSuitable)
           |> Result.iterBoth drawUnavailableReason drawButton

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
                 - 56.0f)

        do this.DrawInfusionList(descView)

        // infuser button
        let infuserView =
            Rect(Vector2(container.center.x - 100.0f, container.yMax - 40.0f), Vector2(200.0f, 36.0f))

        this.DrawApplyButton infuserView
