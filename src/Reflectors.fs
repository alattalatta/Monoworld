module Infusion.Reflectors

open System.Linq.Expressions
open System.Reflection

open Verse

module Projectile =
    type private intendedTarget = System.Func<Projectile, LocalTargetInfo>

    let private localTargetInfoT = typeof<LocalTargetInfo>
    let private projectileT = typeof<Projectile>

    let private param = Expression.Parameter(typeof<obj>)

    let private fieldExp =
        Expression.Field
            (Expression.Convert(param, projectileT),
             projectileT.GetField("intendedTarget", BindingFlags.NonPublic ||| BindingFlags.Instance))

    let private intendedTargetGetter =
        Expression.Lambda<intendedTarget>(Expression.Convert(fieldExp, localTargetInfoT), param).Compile()

    let intendedTargetOf projectile = intendedTargetGetter.Invoke projectile
