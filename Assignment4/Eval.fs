module Interpreter.Eval
    
    open Result
    open Language
    
    let rec aexprToString = 
        function
        | Num x -> string x
        | Add (x1, x2) -> "(" + aexprToString x1 + " + " + aexprToString x2 + ")" 
        | Mul(x1, x2) -> "(" + aexprToString x1 + " * " + aexprToString x2 + ")"
        | Div(x1, x2) -> "(" + aexprToString x1 + " / " + aexprToString x2 + ")"
    let rec bexprToString =
        function
        | TT -> "true"
        | Not x -> "(" +  "not " + bexprToString x + ")"
        | Eq (x1, x2) -> "(" + aexprToString x1 + " = " + aexprToString x2 + ")"
        | Lt (x1, x2) -> "(" + aexprToString x1 + " < " + aexprToString x2 + ")"
        | Conj (b1, b2) -> "(" + bexprToString b1 + " /\ " + bexprToString b2 + ")"
    
    let rec aexprEval a = 
        match a with
        | Num x -> Some x
        | Add (b, c) ->
            match aexprEval b, aexprEval c with
            | Some x, Some y -> Some (x + y)
            | _ -> None
        | Mul (b, c) -> 
            match aexprEval b, aexprEval c with
            | Some x, Some y -> Some (x *y)
            | _ -> None
        | Div (b, c) -> 
            match aexprEval b, aexprEval c with
            | Some x, Some y when y <> 0 -> Some (x / y)
            | _ -> None

    //Option.bind (fun x -> Option.bind (fun y -> Some (x + y)) (aexprEval2 c)) (aexprEval2 b)
    let rec aexprEval2 a =
        match a with
        | Num x -> Some x
        | Add (b, c) -> aexprEval2 b |> Option.bind (fun x -> aexprEval2 c |> Option.bind (fun y -> Some (x + y)))
        | Mul (b, c) -> aexprEval2 b |> Option.bind (fun x -> aexprEval2 c |> Option.bind (fun y -> Some (x * y)))
        | Div (b, c) -> aexprEval2 b |> Option.bind (fun x -> aexprEval2 c |> Option.bind (fun y -> Some (x / y)))
    
    let rec bexprEval b= 
        match b with 
        | TT -> Some true
        | Eq (a, c) -> aexprEval2 a |> Option.bind (fun x -> aexprEval2 c |> Option.bind (fun y -> Some (x = y)))
        | Lt (a, c) -> aexprEval a |> Option.bind (fun x -> aexprEval2 c |> Option.bind (fun y -> Some (x < y)))
        | Conj (b1, b2) -> bexprEval b1 |> Option.bind (fun x -> bexprEval b2 |> Option.bind (fun y -> Some (x && y)))
        | Not a -> bexprEval a |> Option.bind (fun x -> Some (not x))
    
    let rec aux = 
        function
        | Num x -> (0, string x)
        | Add (x1, x2) -> 
            let level = 2
            let (level1, str1) = aux x1
            let (level2, str2) = aux x2
            let left = 
                if level < level1 then
                    "(" + str1 + ")"
                else 
                    str1
            let right = 
                if level < level2 then
                    "(" + str2 + ")"
                else 
                    str2

            level, left + " + " + right

        | Mul (x1, x2) -> 
            let level = 1
            let (level1, str1) = aux x1
            let (level2, str2) = aux x2
            let left = 
                if level < level1 then
                    "(" + str1 + ")"
                else 
                    str1
            let right = 
                if level < level2 then
                    "(" + str2 + ")"
                else 
                    str2
            
            level, left + " * " + right
        | Div (x1, x2) -> 
            let level = 1
            let (level1, str1) = aux x1
            let (level2, str2) = aux x2
            let left =
                if level < level1 then
                    "(" + str1 + ")"
                else 
                    str1
            let right =
                if level <= level2 then
                    "(" + str2 + ")"
                else
                    str2
            level, left + " / " + right
    
    let rec aexprToString2 a =
        match aux a with
        | (_, str) -> str

    let rec aux2 =
        function
        | TT -> (0, "true")
        | Not x ->
            let level = 1
            let (level1, expr) = aux2 x

            let result = 
                if level <= level1 then
                    "(" + expr + ")"
                else 
                    expr
            level, "not " + result
        | Eq (x1, x2) ->
            let level = 2
            let left = aexprToString2 x1
            let right = aexprToString2 x2

            level, left + " = " + right
        | Lt (x1, x2) ->
            let level = 2
            let left = aexprToString2 x1
            let right = aexprToString2 x2

            level, left + " < " + right
        | Conj (x1, x2) ->
            let level = 3
            let (level1, bex1) = aux2 x1
            let (level2, bex2) = aux2 x2

            level, bex1 + " /\ " + bex2
    let bexprToString2 b = 
        match aux2 b with
        | (_, str) -> str

    let aexprFold _ = failwith "not implemented"
    let bexprFold _ = failwith "not implemented"
    
    let aexprToString3 _ = failwith "not implemented"
    let bexprToString3 _ = failwith "not implemented"
    let aexprEval3 _ = failwith "not implemented"
    let bexprEval3 _ = failwith "not implemented"
    let aexprToString4 _ = failwith "not implemented"
    let bexprToString4 _ = failwith "not implemented"