module Fox.Parser

open AST
open FParsec
open System

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

// Utilities
let ws = spaces
let str_ws s = pstring s .>> ws
let between_ws l r p = between (str_ws l) (str_ws r) p
let sepBy_ws p sep = sepBy (p .>> ws) (str_ws sep)

// Base parsers
let pIdentifier = 
    let isFirstChar c = isLetter c || c = '_'
    let isNextChar c = isLetter c || isDigit c || c = '_' || c = '\''
    many1Satisfy2L isFirstChar isNextChar "identifier" .>> ws

let pOperator = 
    many1Satisfy (isAnyOf "+-*/%=<>!&|^~") .>> ws |>> Operator

let pStringLiteral =
    let normalChar = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
        | 'n' -> "\n" | 'r' -> "\r" | 't' -> "\t" | c -> string c)
    between (pstring "\"") (pstring "\"") (stringsSepBy normalChar escapedChar) .>> ws

// Value parsers
let pInt = pint32 .>> ws |>> Int
let pFloat = pfloat .>> ws |>> Float
let pBool = (stringReturn "true" (Bool true)) <|> (stringReturn "false" (Bool false)) .>> ws
let pString = pStringLiteral |>> String
let pNone = str_ws "None" >>% None()
let pLiteral = str_ws "%" >>. pIdentifier |>> Literal

// Expression parser (forward declaration)
let pExpr, pExprRef = createParserForwardedToRef<Expr, unit>()

// Complex expressions
let pVar = pIdentifier |>> Var
let pList = between_ws "[" "]" (sepBy_ws pExpr ",") |>> List
let pTuple = between_ws "(" ")" (sepBy_ws pExpr ",") |>> Tuple

// Function-related parsers
let pLambda =
    pipe2
        (str_ws "\\" >>. pIdentifier .>> str_ws "->")
        pExpr
        (fun param body -> Lambda(param, body))

let pApply =
    pipe2
        (pExpr .>> ws)
        (many1 (pExpr .>> ws))
        (fun func args -> App(func, args))

// Control flow
let pIf =
    pipe3
        (str_ws "if" >>. pExpr .>> str_ws "then")
        pExpr
        (str_ws "else" >>. pExpr)
        (fun cond thenExpr elseExpr -> If(cond, thenExpr, elseExpr))

// Let bindings
let pLet =
    pipe3
        (str_ws "let" >>. pIdentifier .>> ws)
        (many (pIdentifier .>> ws))
        (str_ws "=" >>. pExpr)
        (fun name params' expr -> 
            match params' with
            | [] -> Let(name, expr, None())
            | _ -> 
                let lambda = List.foldBack (fun param acc -> Lambda(param, acc)) params' expr
                Let(name, lambda, None()))

let pLetIn =
    pipe2
        (pLet .>> str_ws "in")
        pExpr
        (fun (Let(name, value, _)) body -> Let(name, value, Some(body)))

// Lazy evaluation
let pLazy = str_ws "lazy" >>. pExpr |>> LazyExpr
let pForce = str_ws "force" >>. pExpr |>> Force

// Pattern matching
let pPattern, pPatternRef = createParserForwardedToRef<Pattern, unit>()

let pMatchCase =
    pipe2
        (pPattern .>> str_ws "->")
        pExpr
        (fun pattern expr -> (pattern, expr))

let pMatch =
    pipe2
        (str_ws "match" >>. pExpr .>> str_ws "with")
        (many1 (str_ws "|" >>. pMatchCase))
        (fun expr cases -> Match(expr, cases))

// Patterns
let pWildcard = str_ws "_" >>% PWildcard
let pPVar = pIdentifier |>> PVar
let pPTuple = between_ws "(" ")" (sepBy_ws pPattern ",") |>> PTuple

do pPatternRef := choice [
    pWildcard
    pPVar
    pPTuple
]

// Type system
let pType, pTypeRef = createParserForwardedToRef<Expr, unit>()

let pBaseType = 
    ["int"; "float"; "bool"; "string"; "unit"; "lazy"]
    |> List.map (fun t -> str_ws t >>% BaseType(t))
    |> choice

let pTypeTuple = between_ws "(" ")" (sepBy_ws pType ",") |>> Type

do pTypeRef := choice [
    pBaseType
    pTypeTuple
]

// Complete expression parser
do pExprRef := choice [
    attempt pFloat
    attempt pInt
    attempt pString
    attempt pBool
    attempt pNone
    attempt pLiteral
    attempt pIf
    attempt pLetIn
    attempt pLet
    attempt pLambda
    attempt pLazy
    attempt pForce
    attempt pMatch
    attempt pList
    attempt pTuple
    attempt pApply
    pVar
]

// Top-level parsers
let pImport = str_ws "import" >>. pIdentifier |>> Import
let pDefinition = pLet <|> (pType |>> DefineConstraint)
let pStatement = choice [pDefinition; pExpr] .>> ws .>> optional (str_ws ";")

let pProgram = many pStatement

// Public interface
let parseProgram input =
    match run pProgram input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwithf "Parse error: %s" errorMsg

let parseExpression input =
    match run pExpr input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwithf "Parse error: %s" errorMsg
