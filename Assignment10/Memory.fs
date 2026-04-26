module Interpreter.Memory
    
    type memory = {
        map : Map<int,int>
        next : int
    } // put your own type here

    let empty (memSize:int) = {
        map = Map.empty
        next = 0
    }
    let alloc size mem = 
        match size with
        | x when size > 0 -> 
            let range = [mem.next .. mem.next + x - 1]
            let mem' = {map = range |> List.fold (fun acc addr -> Map.add addr 0 acc) mem.map ; next = mem.next + x}
            Some (mem', mem.next)
        | _ -> None

    let free ptr size mem = 
        let range = [ptr .. ptr + size - 1]
        let pred = range |> List.forall (fun k -> mem.map.ContainsKey(k))
        if pred then
            let newMap = 
                range |> List.fold (fun acc element -> Map.remove element acc) mem.map
            
            Some {map = newMap ; next = mem.next}
        else 
            None

    let setMem ptr v mem =
        if mem.map.ContainsKey(ptr) then
            Some {map = mem.map.Add(ptr,v) ; next = mem.next}
        else
            None
        
    let getMem ptr mem =
        if mem.map.ContainsKey(ptr) then
            Some mem.map.[ptr]
        else
            None