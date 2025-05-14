module Fox.Utils

open AST
open FParsec
open System

// Улучшенный парсер значений
module ValueParser =
    let private skipWS = spaces
    let private symbol s = pstring s .>> skipWS
    
    let private numberParser =
        let numberFormat = NumberLiteralOptions.AllowMinusSign
                           ||| NumberLiteralOptions.AllowFraction
        numberLiteral numberFormat "number"
        |>> fun nl ->
            if nl.IsInteger then Int(int32 nl.String)
            else Float(float nl.String)
        .>> skipWS

    let private stringParser =
        let normalChar = manySatisfy (fun c -> c <> '\\' && c <> '"')
        let escapedChar = 
            pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                | 'n' -> "\n" | 'r' -> "\r" | 't' -> "\t" | c -> string c)
        between (pstring "\"") (pstring "\"") 
                (stringsSepBy normalChar escapedChar)
        .>> skipWS |>> String

    let private tupleParser, tupleParserRef = 
        createParserForwardedToRef<Expression, unit>()

    let private exprParser = choice [
        numberParser
        stringParser
        tupleParser
    ]

    do tupleParserRef :=
        between (symbol "(") (symbol ")") 
               (sepBy1 exprParser (symbol ","))
        |>> Tuple

    let parse input =
        match run exprParser input with
        | Success(res, _, _) -> res
        | Failure(_, _, _) -> None()

// Оптимизированные операции с коллекциями
module Collections =
    let combine firstMap secondMap =
        Map.fold (fun acc key value -> 
            match Map.tryFind key acc with
            | Some _ -> failwithf "Ключ '%s' уже существует" key
            | None -> Map.add key value acc
        ) firstMap secondMap

    let containsValue value sequence =
        let rec search = function
            | [] -> false
            | head::tail when head = value -> true
            | _::tail -> search tail
        search sequence

// Усовершенствованная свертка с прерыванием
module Folding =
    type FoldResult<'T> =
        | Continue of 'T
        | Break of 'T

    let foldWhile initial folder sequence =
        let rec processItems state = function
            | [] -> state
            | item::rest ->
                match folder state item with
                | Break result -> result
                | Continue newState -> processItems newState rest
        processItems initial sequence

// Компактные утилиты для кортежей
[<AutoOpen>]
module TupleShortcuts =
    let (||>) a b = (a, b)
    let (|||>) a b c = (a, b, c)
    let (||||>) a b c d = (a, b, c, d)
    let (|||||>) a b c d e = (a, b, c, d, e)
