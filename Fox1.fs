
type Expr =
    // Базовые конструкции
    | Int of int
    | Bool of bool
    | Str of string
    | Var of string
    | Lambda of string * Expr
    | App of Expr * Expr list     
    | Let of string * Expr * Expr
    | LetRec of string * Expr * Expr
    | If of Expr * Expr * Expr
    | LazyExpr of Expr
    | Force of Expr
    | List of Expr list
    | Tuple of Expr list
    
    // Система типов и ограничений
    | Type of string
    | TypeConstraintOr of Expr list
    | TypeConstraintTuple of Expr list
    | TypeWildcard
    
    | Match of Expr * (Pattern * Expr) list
    | MatchAny
    
    // Ввод-вывод
    | ReadFile of string
    | WriteFile of string * Expr
    | Print of Expr
    
    // Специальные конструкции
    | Closure of string * Expr * Env
    | Thunk of Expr * Env

and Pattern =
    | PVar of string
    | PType of string
    | PTuple of Pattern list
    | PWildcard

and Env = Map<string, Expr>

// Система типов
type LazyType =
    | TInt
    | TBool
    | TStr
    | TList of LazyType
    | TTuple of LazyType list
    | TLambda of LazyType * LazyType
    | TThunk of LazyType
    | TVar of string  

let rec typeInfer expr env =
    match expr with
    | Int(_) -> TInt
    | Bool(_) -> TBool
    | Str(_) -> TStr
    | Var(x) -> 
        match Map.tryFind x env with
        | Some(t) -> t
        | None -> failwith $"Undefined variable {x}"
    
    | Lambda(param, body) ->
        let paramType = TVar $"'{param}"  
        let newEnv = Map.add param paramType env
        TLambda(paramType, typeInfer body newEnv)
    
    | App(func, args) ->
        let funcType = typeInfer func env
        let argTypes = List.map (fun a -> typeInfer a env) args
        match funcType with
        | TLambda(input, output) when input = (TTuple argTypes) -> output
        | _ -> failwith "Type mismatch in function application"
    
    | LazyExpr(e) -> TThunk(typeInfer e env)
    | Force(e) -> 
        match typeInfer e env with
        | TThunk(t) -> t
        | _ -> failwith "Forcing non-thunk expression"
    
    | List(elems) ->
        let elemTypes = List.map (typeInfer) elems
        let unifiedType = List.reduce unifyTypes elemTypes
        TList(unifiedType)
    
    | Tuple(elems) -> TTuple(List.map (typeInfer) elems)
    
    | Match(expr, cases) ->
        let exprType = typeInfer expr env
        List.iter (fun (pat, body) ->
            let patEnv = checkPattern pat exprType env
            typeInfer body patEnv |> ignore) cases
        exprType  // Возвращаем тип исходного выражения
    
    | _ -> failwith "Type inference not implemented for this construct"

and unifyTypes t1 t2 =
    match (t1, t2) with
    | (TVar a, TVar b) when a = b -> t1
    | (TVar _, t) | (t, TVar _) -> t
    | (TList(inner1), TList(inner2)) -> TList(unifyTypes inner1 inner2)
    | (TTuple(types1), TTuple(types2)) when List.length types1 = List.length types2 ->
        TTuple(List.map2 unifyTypes types1 types2)
    | _ when t1 = t2 -> t1
    | _ -> failwith $"Type mismatch: {t1} vs {t2}"

// Проверка соответствия шаблона
let rec checkPattern pattern expectedType env =
    match pattern with
    | PVar name -> Map.add name expectedType env
    | PType typeName when typeToString expectedType = typeName -> env
    | PTuple patterns ->
        match expectedType with
        | TTuple types when List.length types = List.length patterns ->
            List.fold2 (fun acc p t -> checkPattern p t acc) env patterns types
        | _ -> failwith "Tuple pattern mismatch"
    | PWildcard -> env
    | _ -> failwith "Pattern matching not implemented"

let typeToString = function
    | TInt -> "int"
    | TBool -> "bool"
    | TStr -> "string"
    | TList(t) -> $"list<{typeToString t}>"
    | TTuple(ts) -> $"({String.concat "*" (List.map typeToString ts)})"
    | TLambda(t1, t2) -> $"{typeToString t1} -> {typeToString t2}"
    | TThunk(t) -> $"lazy<{typeToString t}>"
    | TVar name -> name
