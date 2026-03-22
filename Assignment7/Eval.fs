module Interpreter.Eval

    open Result
    open Language
    open State

    let rec readInt() = 
        let readFromConsole = System.Console.ReadLine().Trim()
        let tryParseInt (str: string) = System.Int32.TryParse str
        let input = tryParseInt readFromConsole

        match input with
        | (true, result) -> result
        | (false, result) -> 
            printfn "%d is not an integer" result 
            readInt()
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
        | Random -> Some (random st)
        | Read -> Some (readInt())
        | Cond(b, a1, a2) ->
            match boolEval b st with
            | Some true -> 
                match arithEval a1 st with
                | Some x -> Some x
                | None -> None
            | Some false -> 
                match arithEval a2 st with
                | Some x -> Some x
                | None -> None
            | None -> None
    and boolEval b st =
        match b with
        | TT -> Some true
        | Not x -> boolEval x st |> Option.bind (fun x -> Some (not x))
        | Eq(x1, x2) -> arithEval x1 st |> Option.bind (fun x -> arithEval x2 st |> Option.bind (fun y -> Some (x = y)))
        | Lt(x1, x2) -> arithEval x1 st |> Option.bind (fun x -> arithEval x2 st |> Option.bind (fun y -> Some (x < y)))
        | Conj(b1, b2) -> boolEval b1 st |> Option.bind (fun x -> boolEval b2 st |> Option.bind (fun y -> Some (x && y)))
    
    let rec mergeStrings (es:aexpr list) s st =
        let split (s1:string) (s2:string) = s2 |> s1.Split |> Array.toList
        let splitResult = split s "%"

        if es.Length <> splitResult.Length - 1 then
            None
        else
            let values = es |> List.map (fun e -> arithEval e st)

            if values |> List.exists Option.isNone then
                None
            else
                let nums = values |> List.map Option.get

                let rec aux acc splitResult nums  =
                    match splitResult, nums with
                    | [lastPart], [] -> Some (acc + lastPart)
                    | part :: ps, n :: ns -> aux (acc + part + string n) ps ns 
                    | _ -> None
                
                aux "" splitResult nums

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
        | Print (es, s) -> 
            match mergeStrings es s st with
            | Some str -> 
                printfn "%s" str
                Some st
            | None -> None
     
    let rec mergeStrings2 (es : aexpr list) s st =
        let split (s1:string) (s2:string) = s2 |> s1.Split |> Array.toList
        let splitResult = split s "%"

        if es.Length <> splitResult.Length - 1 then
            None
        else
            let values = es |> List.map (fun e -> arithEval e st)

            if values |> List.exists Option.isNone then
                None
            else
                let nums = values |> List.map Option.get

                let rec aux splitResult values c = 
                    match splitResult, values with
                    | [last], [] -> c last
                    | part :: ps, v :: vs -> aux ps vs (fun r -> c (part + string v + r))
                    | _ -> None
                
                aux splitResult values (fun r -> Some r)    