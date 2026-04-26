module Interpreter.Eval

    open Result
    open StateMonad
    open Language

    let rec readInt() = 
        let readFromConsole = System.Console.ReadLine().Trim()
        let tryParseInt (str: string) = System.Int32.TryParse str
        let input = tryParseInt readFromConsole

        match input with
        | true, result -> result
        | false, result -> 
            printfn "%d is not an integer" result 
            readInt()
    
    let rec aexprEval a = 
        match a with
        | Num x -> ret x
        | Var v -> getVar v
        | Add (x1, x2) -> aexprEval x1 >>= fun x -> aexprEval x2 >>= fun y -> ret (x + y)
        | Mul (x1, x2) -> aexprEval x1 >>= fun x -> aexprEval x2 >>= fun y -> ret (x * y)
        | Div (x1, x2) -> aexprEval x1 >>= fun x -> aexprEval x2 >>= fun y -> if y <> 0 then ret (x / y) else fail
        | MemRead (a) -> aexprEval a >>= fun x -> getMem x
        | Cond (b, a1, a2) -> bexprEval b >>= fun b -> if b then aexprEval a1 else aexprEval a2
        | Random -> random
        | Read -> ret (readInt())
    and bexprEval b =
        match b with
        | TT -> ret true
        | Eq (a1, a2) -> aexprEval a1 >>= fun x -> aexprEval a2 >>= fun y -> ret (x = y)
        | Lt (a1, a2) -> aexprEval a1 >>= fun x -> aexprEval a2 >>= fun y -> ret (x < y)
        | Conj (b1, b2) -> bexprEval b1 >>= fun x -> bexprEval b2 >>= fun y -> ret (x && y)
        | Not b -> bexprEval b >>= fun x -> ret (not x)

    type StateBuilder() =
        member this.Bind(f, x) = (>>=) f x
        member this.Return(x) = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)
    
    let eval = StateBuilder()
    let rec aexprEval2 a =
        match a with
        | Num x ->
            eval {
                return x
            }
        | Var v -> 
            eval {
                return! getVar v
            }
        | Add (x1, x2) ->
            eval {
                let! x = aexprEval2 x1
                let! y = aexprEval2 x2
                return x + y
            }
        | Mul (x1, x2) ->
            eval {
                let! x = aexprEval2 x1
                let! y = aexprEval2 x2
                return x * y
            }
        | Div (x1, x2) -> 
            eval {
                let! x = aexprEval2 x1
                let! y = aexprEval2 x2
                if y <> 0 then
                    return x / y
                else
                    return! fail
            }
        | MemRead (a) ->
            eval {
                let! x = aexprEval2 a
                return! getMem x
            }
        | Cond (x, a1, a2) -> 
            eval {
                let! b = bexprEval2 x

                if b then
                    return! aexprEval2 a1
                else 
                    return! aexprEval2 a2
            }
        | Random -> 
            eval {
                return! random
            }
        | Read -> 
            eval {
                return! ret(readInt())
            }
    and bexprEval2 b =
        match b with
        | TT -> eval { return true}
        | Eq (a1, a2) ->
            eval {
                let! x = aexprEval2 a1
                let! y = aexprEval2 a2
                return x = y
            }
        | Lt (a1, a2) ->
            eval {
                let! x = aexprEval2 a1
                let! y = aexprEval2 a2
                return x < y
            }
        | Conj (b1, b2) ->
            eval {
                let! x = bexprEval2 b1
                let! y = bexprEval2 b2
                return x && y
            }
        | Not b -> 
            eval { 
                let! x = bexprEval2 b 
                return not x
            }

    let rec mergeStrings2 (es : aexpr list) s =
        let split (s1:string) (s2:string) = s2 |> s1.Split |> Array.toList
        let splitResult = split s "%"

        let rec aux a =
                match a with
                | [] -> ret []
                | x :: xs -> aexprEval2 x >>= fun v -> aux xs >>= fun vs -> ret (v :: vs)

        if es.Length <> splitResult.Length - 1 then
            fail
        else
            aux es >>= fun values ->
                let rec aux2 splitResult values c = 
                        match splitResult, values with
                        | [last], [] -> c last
                        | part :: ps, v :: vs -> aux2 ps vs (fun r -> c (part + string v + r))
                        | _ -> None
                
                match aux2 splitResult values (fun r -> Some r) with
                | Some result -> ret result
                | None -> fail
    let rec stmntEval s = 
        match s with
        | Skip -> ret ()
        | Declare v -> declare v
        | Assign (v, a) -> aexprEval a >>= fun x -> setVar v x
        | Seq (s1, s2) -> stmntEval s1 >>= fun x -> stmntEval s2
        | If (b, s1, s2) -> 
            bexprEval b >>= fun v ->
                if v then
                    stmntEval s1
                else
                    stmntEval s2
        | While (b, s) ->
            bexprEval b >>= fun x ->
                if x then
                    stmntEval s >>= fun _ -> stmntEval (While (b, s))
                else 
                    ret ()
        | Alloc (x, a) -> aexprEval a >>= fun z -> alloc x z
        | Free (a1, a2) -> aexprEval a1 >>= fun x -> aexprEval a2 >>= fun y -> free x y
        | MemWrite (a1, a2) -> aexprEval a1 >>= fun x -> aexprEval a2 >>= fun y -> setMem x y
        | Print (lst, s) -> mergeStrings2 lst s >>= fun str -> ret (printfn "%s" str)
