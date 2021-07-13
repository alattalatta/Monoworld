module Infusion.Reflectors

open System.Linq.Expressions

open HarmonyLib
open RimWorld
open Verse


module Reflectors =
    let param = Expression.Parameter(typeof<obj>)

    let fieldGetter<'klass, 'field> classType fieldType fieldName =
        let fieldExp =
            Expression.Field(Expression.Convert(param, classType), AccessTools.Field(classType, fieldName))

        let getter =
            Expression.Lambda<System.Func<'klass, 'field>>(Expression.Convert(fieldExp, fieldType), param).Compile()

        let getField klass =
            try
                getter.Invoke klass |> Ok
            with ex -> Error ex

        getField


// What the
module StatWorker =
    let getStat statWorker =
        Reflectors.fieldGetter<StatWorker, StatDef> typeof<StatWorker> typeof<StatDef> "stat" statWorker
