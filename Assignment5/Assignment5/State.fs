module Interpreter.State

    open Result
    open Language

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
        variables : Map<string,int>
    } 
    
    let mkState () =  {variables = Map.empty}
    
    //st with variables = 
    let declare s st = 
        if not(reservedVariableName s) && validVariableName s && not(st.variables.ContainsKey(s)) then
            Some {variables = st.variables.Add(s,0)}
        else
            None
        
    let getVar x st = 
        if st.variables.ContainsKey(x) then
            Some st.variables[x]
        else
            None
    let setVar x v st= 
        if st.variables.ContainsKey(x) then
            Some {variables = st.variables.Add(x,v)}
        else
            None
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     