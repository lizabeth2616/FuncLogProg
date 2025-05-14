module Fox.Math

open AST
open Interpreter

/// Базовый тип для математических функций
type MathFunction = Expr list -> Environment -> Expr

/// Обёртка для встроенных математических функций
let private mathFunctionWrapper (f: float -> float) : MathFunction =
    fun args env ->
        match args with
        | [Float x] -> Float(f x)
        | [Int x] -> Float(f (float x))
        | [LazyExpr e] -> 
            let evaluated = evaluate e env
            match evaluated with
            | Float x -> Float(f x)
            | Int x -> Float(f (float x))
            | _ -> failwith "Неподдерживаемый тип для ленивого вычисления"
        | _ -> failwith "Неверное количество аргументов"

/// Проверка и преобразование типов для математических операций
let private validateNumber = function
    | Int x -> float x
    | Float x -> x
    | LazyExpr e -> 
        match evaluate e Map.empty with
        | Int x -> float x
        | Float x -> x
        | _ -> failwith "Ожидалось численное значение"
    | _ -> failwith "Нечисловой аргумент"

// Реализации основных математических функций
let sqrtImpl = mathFunctionWrapper sqrt
let absImpl = mathFunctionWrapper abs
let sinImpl = mathFunctionWrapper sin
let cosImpl = mathFunctionWrapper cos
let tanImpl = mathFunctionWrapper tan

/// Система типов для математических функций
let mathTypeSystem = 
    Map.empty
    |> Map.add "sqrt" (TLambda(TNumber, TNumber))
    |> Map.add "abs" (TLambda(TNumber, TNumber))
    |> Map.add "sin" (TLambda(TNumber, TNumber))
    |> Map.add "cos" (TLambda(TNumber, TNumber))
    |> Map.add "tan" (TLambda(TNumber, TNumber))

/// Модуль математических функций
let mathModule =
    Map.empty
    |> Map.add "sqrt" (BuiltinFunc sqrtImpl)
    |> Map.add "abs" (BuiltinFunc absImpl)
    |> Map.add "sin" (BuiltinFunc sinImpl)
    |> Map.add "cos" (BuiltinFunc cosImpl)
    |> Map.add "tan" (BuiltinFunc tanImpl)
    |> Map.add "pi" (Float System.Math.PI)
    |> Map.add "e" (Float System.Math.E)

// Интеграция с системой типов
let integrateWithTypeSystem env =
    { env with 
        Static = Map.union env.Static mathTypeSystem
        Dynamic = Map.union env.Dynamic mathModule
    }
