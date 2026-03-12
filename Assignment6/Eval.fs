module Interpreter.Eval

    open Result
    open Language
    open State

    let rec arithEval a st =
        match a with 
        | Num x -> Some x
        | Var v  -> getVar v st
        | Add (x1, x2) -> 
            arithEval x1 st |> Option.bind (fun x -> arithEval x2 st |> Option.bind (fun y -> Some (x + y)))
        | Mul (x1, x2) ->
            arithEval x1 st |> Option.bind (fun x -> arithEval x2 st |> Option.bind (fun y -> Some (x * y)))
        | Div (x1, x2) ->
            arithEval x1 st |> Option.bind (fun x -> arithEval x2 st |> Option.bind (fun y -> if y <> 0 then Some(x / y) else None))
        | MemRead e1 -> 
            match arithEval e1 st with
            | Some ptr -> getMem ptr st
            | _ -> None
    
    let rec boolEval b st =
        match b with
        | TT -> Some true
        | Not x -> boolEval x st |> Option.bind (fun x -> Some (not x))
        | Eq(x1, x2) -> arithEval x1 st |> Option.bind (fun x -> arithEval x2 st |> Option.bind (fun y -> Some (x = y)))
        | Lt(x1, x2) -> arithEval x1 st |> Option.bind (fun x -> arithEval x2 st |> Option.bind (fun y -> Some (x < y)))
        | Conj(b1, b2) -> boolEval b1 st |> Option.bind (fun x -> boolEval b2 st |> Option.bind (fun y -> Some (x && y)))
    
    let rec stmntEval s st = 
        match s with
        | Skip -> Some st
        | Declare v  -> declare v st 
        | Assign (v, a) ->
            match arithEval a st with
            | Some x-> setVar v x st
            | _ -> None
        | Seq (s1, s2) ->
            match stmntEval s1 st with
            | Some st -> stmntEval s2 st
            | _ -> None
        | If (b, s1, s2) -> 
            match boolEval b st with
            | Some true -> stmntEval s1 st
            | Some false -> stmntEval s2 st
            | _ -> None
        | While (b, s) ->
            match boolEval b st with
            | Some true ->
                match stmntEval s st with
                | Some x' -> stmntEval (While(b, s)) x'
                | _ -> None
            | Some false -> Some st
            | _ -> None
        | Alloc (x, e) -> 
            match arithEval e st with
            | Some size -> alloc x size st
            | _ -> None
        | Free (e1, e2) -> 
            match arithEval e1 st, arithEval e2 st with
            | Some ptr, Some size -> free ptr size st
            | _ -> None
        | MemWrite (e1, e2) ->
            match arithEval e1 st, arithEval e2 st with
            | Some ptr, Some v -> setMem ptr v st
            | _ -> None