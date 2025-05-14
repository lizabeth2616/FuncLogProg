module Fox.StdModule

open AST
open System
open System.IO

// Вспомогательные функции для обработки значений
let private asInt = function
    | Int x -> x
    | Float x -> int x
    | String s -> Int32.Parse s
    | Bool true -> 1
    | Bool false -> 0
    | _ -> failwith "Cannot convert to int"

let private asFloat = function
    | Float x -> x
    | Int x -> float x
    | String s -> Double.Parse s
    | Bool true -> 1.0
    | Bool false -> 0.0
    | _ -> failwith "Cannot convert to float"

let private asBool = function
    | Bool b -> b
    | Int 0 -> false
    | Int _ -> true
    | Float 0.0 -> false
    | Float _ -> true
    | String "" -> false
    | String _ -> true
    | None() -> false
    | _ -> true

// Базовые арифметические операции
let private mathOp op args =
    match args with
    | [Int a; Int b] -> Int (op a b)
    | [Float a; Float b] -> Float (op a b)
    | [a; b] -> Float (op (asFloat a) (asFloat b))
    | _ -> failwith "Invalid arguments for math operation"

let private stdPlus = InternalFunction(function
    | [String a; String b] -> String (a + b)
    | args -> mathOp (+) args)

let private stdMinus = InternalFunction(function
    | [a] -> mathOp (-) [Int 0; a]
    | args -> mathOp (-) args)

let private stdMult = InternalFunction(mathOp (*))
let private stdDiv = InternalFunction(mathOp (/))
let private stdMod = InternalFunction(mathOp (%))

// Логические операции
let private boolOp op args =
    match args with
    | [a; b] -> Bool (op (asBool a) (asBool b))
    | _ -> failwith "Invalid arguments for bool operation"

let private stdAnd = InternalFunction(boolOp (&&))
let private stdOr = InternalFunction(boolOp (||))
let private stdNot = InternalFunction(function
    | [a] -> Bool (not (asBool a))
    | _ -> failwith "Not expects one argument")

// Операции сравнения
let private cmpOp op args =
    match args with
    | [Int a; Int b] -> Bool (op a b)
    | [Float a; Float b] -> Bool (op a b)
    | [String a; String b] -> Bool (op a b)
    | [Bool a; Bool b] -> Bool (op a b)
    | [a; b] -> Bool (op (asFloat a) (asFloat b))
    | _ -> failwith "Comparison needs two arguments"

let private stdLt = InternalFunction(cmpOp (<))
let private stdGt = InternalFunction(cmpOp (>))
let private stdLte = InternalFunction(cmpOp (<=))
let private stdGte = InternalFunction(cmpOp (>=))
let private stdEq = InternalFunction(cmpOp (=))
let private stdNeq = InternalFunction(cmpOp (<>))

// Функции ввода-вывода
let private stdPrint = InternalFunction(function
    | [] -> None()
    | args ->
        args |> List.iter (function
            | Int x -> printf "%d" x
            | Float x -> printf "%f" x
            | String s -> printf "%s" s
            | Bool b -> printf "%b" b
            | None() -> printf "None"
            | Literal "nl" -> printfn ""
            | Literal "ws" -> printf " "
            | Tuple items -> 
                printf "("
                items |> List.iteri (fun i x ->
                    stdPrint.Function [x] |> ignore
                    if i < items.Length - 1 then printf ", ")
                printf ")"
            | _ -> failwith "Cannot print value")
        None())

let private stdReadLine = InternalFunction(function
    | [] -> String (Console.ReadLine())
    | _ -> failwith "readLine expects no arguments")

// Функции преобразования типов
let private stdToInt = InternalFunction(function
    | [x] -> Int (asInt x)
    | _ -> failwith "toInt expects one argument")

let private stdToFloat = InternalFunction(function
    | [x] -> Float (asFloat x)
    | _ -> failwith "toFloat expects one argument")

// Специальные функции
let private stdId = InternalFunction(function
    | [x] -> x
    | [] -> None()
    | _ -> failwith "id expects zero or one argument")

let private stdCompose = InternalFunction(function
    | [f; g] ->
        match f, g with
        | Lambda(p1, e1), Lambda(p2, e2) -> Lambda(p1, App(g, [App(f, [Var p1])]))
        | _ -> failwith "Composition expects two functions"
    | _ -> failwith "Composition expects two arguments")

let private stdMatchType = InternalFunction(function
    | [x; t] -> Bool (TypeSatisfyConstraint (TypeOf x) t)
    | _ -> failwith "matchType expects two arguments")

// Стандартный модуль
let STD_MODULE = Map [
    // Арифметические операторы
    "+", stdPlus
    "-", stdMinus
    "*", stdMult
    "/", stdDiv
    "%", stdMod
    
    // Операторы сравнения
    "<", stdLt
    ">", stdGt
    "<=", stdLte
    ">=", stdGte
    "=", stdEq
    "!=", stdNeq
    
    // Логические операторы
    "&&", stdAnd
    "||", stdOr
    "!", stdNot
    
    // Специальные операторы
    ">>", stdCompose
    
    // Стандартные функции
    "std.print", stdPrint
    "std.read_line", stdReadLine
    "std.to_int", stdToInt
    "std.to_float", stdToFloat
    "std.id", stdId
    "std.match_type", stdMatchType
]
