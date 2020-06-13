namespace Infusion.Complex

open Verse

open Infusion


type ToolCapacity =
    inherit Complex<InfusionDef>

    val mutable defs: ResizeArray<ToolCapacityDef>

    new() =
        { inherit Complex<InfusionDef>()
          defs = ResizeArray() }

    member this.DefNames = this.defs |> Seq.map (fun def -> def.defName)

    override this.Match thing _ =
        if thing.def.IsApparel || this.defs.Count = 0 then
            true
        else
            thing.def.tools
            |> Seq.reduce (fun a b -> if a.power > b.power then a else b)
            |> (fun tool ->
                tool.capacities
                |> Seq.exists (fun capacity -> Seq.contains capacity.defName this.DefNames))

    override this.BuildRequirementString() =
        if this.defs.Count = 0 then
            None
        else
            this.defs
            |> Seq.map (fun def -> def.label)
            |> String.concat ", "
            |> Some
