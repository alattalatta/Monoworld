namespace Infusion

open Verse

[<AllowNullLiteral>]
type HashEqualDef() =
    inherit Def()

    override this.Equals(ob: obj) =
        match ob with
        | :? HashEqualDef as def -> this.defName = def.defName
        | _ -> false

    override this.GetHashCode() = this.defName.GetHashCode()
