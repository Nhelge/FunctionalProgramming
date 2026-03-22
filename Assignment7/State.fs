module Interpreter.State

    open Result
    open Language
    open Memory
    
    let reservedVariableName v = 
        ["if";"then";"else";"while";"declare";"print";"random";"fork";"__result__"] 
        |> List.exists(fun x -> x = v)
    let validVariableName (v:string) = 
        not(System.String.IsNullOrEmpty v) 
        &&
        (
        let first = v.[0]
        (System.Char.IsAsciiLetter first || first = '_')
        &&
        v |> String.forall (fun x -> System.Char.IsAsciiLetterOrDigit x || x = '_')
        )
    
    type state = {
        variables: Map<string,int>
        memory: memory
        random: System.Random
    } // your type goes here
    
    let mkState (memSize:int) oseed = {
        variables = Map.empty
        memory = empty memSize
        random = 
            match oseed with
            | Some seed -> System.Random(seed)
            | None -> System.Random()
    }
    let random st = st.random.Next()
    
    let declare s st = 
        if not(reservedVariableName s) && validVariableName s && not(st.variables.ContainsKey(s)) then
            Some {st with variables = st.variables.Add(s,0)}
        else
            None
        
    let getVar x st = 
        if st.variables.ContainsKey(x) then
            Some st.variables[x]
        else
            None
    let setVar x v st= 
        if st.variables.ContainsKey(x) then
            Some {st with variables = st.variables.Add(x,v)}
        else
            None
    //variable (setVar) x skal pege på gammel next position
    let alloc x size st =
        match alloc size st.memory with
        | Some (mem', ptr) ->
            let st' = {st with memory = mem'}
            setVar x ptr st'
        | None -> None

    let free ptr size st =
        match free ptr size st.memory with
        | Some mem' -> 
            let st' = {st with memory = mem'}
            Some st'
        | None -> None
    let getMem ptr st = getMem ptr st.memory
    let setMem ptr v st =
        match setMem ptr v st.memory with
        | Some mem' -> 
            let st' = {st with memory = mem'}
            Some st'
        | None -> None
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     