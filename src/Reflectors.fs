module Infusion.Reflectors

open System.Linq.Expressions
open System.Reflection

open RimWorld
open Verse


module Reflectors =
    let param = Expression.Parameter(typeof<obj>)

    let fieldGetter<'klass, 'field> classType fieldType fieldName =
        let fieldExp =
            Expression.Field
                (Expression.Convert(param, classType),
                 classType.GetField(fieldName, BindingFlags.NonPublic ||| BindingFlags.Instance))

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


// Why would you be private to me
module Projectile =
    let getIntendedTarget projectile =
        Reflectors.fieldGetter<Projectile, LocalTargetInfo> typeof<Projectile> typeof<LocalTargetInfo> "intendedTarget"
            projectile
