module Interpreter.Eval

    open Result
    open Language
    open State

    let rec aexprEval a (st:state) =
        match a with 
        | Num x -> Some x
        | Var v when st.variables.ContainsKey(v) -> Some (st.variables[v])
        | Add (x1, x2) -> 
            aexprEval x1 st |> Option.bind (fun x -> aexprEval x2 st |> Option.bind (fun y -> Some (x + y)))
        | Mul (x1, x2) ->
            aexprEval x1 st |> Option.bind (fun x -> aexprEval x2 st |> Option.bind (fun y -> Some (x * y)))
        | Div (x1, x2) ->
            aexprEval x1 st |> Option.bind (fun x -> aexprEval x2 st |> Option.bind (fun y -> if y <> 0 then Some(x / y) else None))
        | MemRead x -> Some 0
        | _ -> None
    
    let rec bexprEval b st =
        match b with
        | TT -> Some true
        | Not x -> bexprEval x st |> Option.bind (fun x -> Some (not x))
        | Eq(x1, x2) -> aexprEval x1 st |> Option.bind (fun x -> aexprEval x2 st |> Option.bind (fun y -> Some (x = y)))
        | Lt(x1, x2) -> aexprEval x1 st |> Option.bind (fun x -> aexprEval x2 st |> Option.bind (fun y -> Some (x < y)))
        | Conj(b1, b2) -> bexprEval b1 st |> Option.bind (fun x -> bexprEval b2 st |> Option.bind (fun y -> Some (x && y)))
    
    let rec stmntEval s st = 
        match s with
        | Skip -> Some st
        | Declare v  -> declare v st 
        | Assign (v, a) ->
            match aexprEval a st with
            | Some x-> setVar v x st
            | _ -> None
        | Seq (s1, s2) ->
            match stmntEval s1 st with
            | Some st -> stmntEval s2 st
            | _ -> None
        | If (b, s1, s2) -> 
            match bexprEval b st with
            | Some true -> stmntEval s1 st
            | Some false -> stmntEval s2 st
            | _ -> None
        | While (b, s) ->
            match bexprEval b st with
            | Some true ->
                match stmntEval s st with
                | Some x' -> stmntEval (While(b, s)) x'
                | _ -> None
            | Some false -> Some st
            | _ -> None
        | _ -> Some st