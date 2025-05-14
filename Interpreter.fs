module FoxInterpreter

open System
open System.IO
open AST
open FParsec

type Environment = {
    Dynamic: Map<string, Expr>
    Static: Map<string, Expr>
    Thunks: Map<string, Expr * Environment>
}

module Environment =
    let empty = {
        Dynamic = Map.empty
        Static = Map.empty
        Thunks = Map.empty
    }
    
    let mergeStatic newStatic env =
        { env with Static = Map.fold (fun acc k v -> Map.add k v acc) env.Static newStatic }
    
    let addDynamic name value env =
        { env with Dynamic = Map.add name value env.Dynamic }
    
    let addThunk name expr env =
        { env with Thunks = Map.add name (expr, env) env.Thunks }

let rec evaluate expr env =
    match expr with
    // Базовые значения
    | Int n -> Int n
    | Bool b -> Bool b
    | Str s -> Str s
    | List es -> List(List.map (fun e -> evaluate e env) es)
    | Tuple es -> Tuple(List.map (fun e -> evaluate e env) es)
    
    // Ленивые вычисления
    | LazyExpr e ->
        let thunkId = Guid.NewGuid().ToString()
        Environment.addThunk thunkId e env |> ignore
        Thunk(thunkId)
    
    | Force(Thunk id) ->
        match Map.tryFind id env.Thunks with
        | Some (e, savedEnv) ->
            let result = evaluate e savedEnv
            { env with Thunks = Map.remove id env.Thunks }
            |> addDynamic id result
            result
        | None -> failwith $"Thunk {id} not found"
    
    // Работа с переменными
    | Var name ->
        match Map.tryFind name env.Dynamic with
        | Some value -> value
        | None ->
            match Map.tryFind name env.Static with
            | Some value -> value
            | None -> failwith $"Undefined variable {name}"
    
    // Функции и применения
    | Lambda(param, body) -> Closure(param, body, env)
    | App(func, args) ->
        let evaluatedFunc = evaluate func env
        let applyArg arg = evaluate arg env
        
        match evaluatedFunc with
        | Closure(param, body, closureEnv) ->
            let argsEnv = 
                args
                |> List.map applyArg
                |> List.zip [param]
                |> Map.ofList
            
            evaluate body { closureEnv with Dynamic = Map.union closureEnv.Dynamic argsEnv }
        
        | BuiltinFunc f -> f args
        | _ -> failwith "Not a function"
    
    // Управляющие конструкции
    | If(cond, thenExpr, elseExpr) ->
        match evaluate cond env with
        | Bool true -> evaluate thenExpr env
        | Bool false -> evaluate elseExpr env
        | _ -> failwith "Condition must be boolean"
    
    // Рекурсивные определения
    | LetRec(name, expr, body) ->
        let rec newEnv = 
            { env with 
                Dynamic = Map.add name (evaluate expr newEnv) env.Dynamic 
            }
        evaluate body newEnv
    
    // Работа с файлами
    | ReadFile path -> 
        try File.ReadAllText path |> Str
        with _ -> failwith $"File not found: {path}"
    
    | WriteFile(path, contentExpr) ->
        let content = evaluate contentExpr env
        match content with
        | Str text -> 
            File.WriteAllText(path, text)
            Tuple []
        | _ -> failwith "File content must be string"
    
    | Match(target, cases) ->
        let evaluatedTarget = evaluate target env
        let rec tryMatch cases =
            match cases with
            | [] -> failwith "No matching pattern"
            | (pattern, expr)::rest ->
                match matchPattern pattern evaluatedTarget env with
                | Some newEnv -> evaluate expr newEnv
                | None -> tryMatch rest
        
        tryMatch cases
    
    | _ -> failwith $"Unsupported expression: {expr}"

and matchPattern pattern value env =
    match pattern with
    | PVar name -> Some (Environment.addDynamic name value env)
    | PWildcard -> Some env
    | PType typeName when typeCheck value typeName -> Some env
    | PTuple patterns ->
        match value with
        | Tuple values when List.length patterns = List.length values ->
            List.zip patterns values
            |> List.fold (fun acc (p, v) ->
                acc |> Option.bind (fun e -> matchPattern p v e)) (Some env)
        | _ -> None
    | _ -> None

// Проверка типов
let typeCheck expr expectedType =
    let actualType = typeOf expr
    match (actualType, expectedType) with
    | (TThunk t1, TThunk t2) -> t1 = t2
    | (TList t1, TList t2) -> t1 = t2
    | (TTuple ts1, TTuple ts2) -> List.forall2 (=) ts1 ts2
    | (TVar _, _) | (_, TVar _) -> true
    | _ -> actualType = expectedType

// Вывод типов
let rec typeOf expr =
    match expr with
    | Int _ -> TInt
    | Bool _ -> TBool
    | Str _ -> TStr
    | List [] -> TList(TVar "'a")
    | List (x::_) -> TList(typeOf x)
    | Tuple es -> TTuple(List.map typeOf es)
    | LazyExpr e -> TThunk(typeOf e)
    | Closure _ -> TLambda(TVar "'a", TVar "'b")
    | BuiltinFunc _ -> TLambda(TVar "'a", TVar "'b")
    | _ -> TVar "'unknown"
