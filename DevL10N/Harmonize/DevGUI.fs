module DevL10N.Harmonize.DevGUI

open System

open LudeonTK
open HarmonyLib
open Verse
open UnityEngine


[<HarmonyPatch(typeof<DevGUI>, "Label")>]
module Label =
  let Prefix (rect: Rect, label: string) =
    let num = Prefs.UIScale / 2f

    let position =
      if Prefs.UIScale > 1f && Math.Abs(num - Mathf.Floor(num)) > Single.Epsilon then
        UIScaling.AdjustRectToUIScaling(rect)
      else
        rect

    let labelStyled =
      if label.Contains("{") then
        let labelSplit = label.Split([|'{'; '}'|])
        sprintf "%s  <size=12><color=#979797>(%s)</color></size>" labelSplit[0] labelSplit[1]
      else
        label

    GUI.Label(position, " " + labelStyled.TrimStart(), Text.CurFontStyle)

    false
