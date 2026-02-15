module Assignment3
    
    let add5 x = x + 5

    let mul3 x = x * 3

    let add5mul3 x = x |> add5 |> mul3

    let add5mul3_2 = add5 >> mul3
    
    let add5_2 f x = f x + 5

    let mul3_2 f x = f x * 3
   
    let rec downto4 f n e = 
        match n with
        | n when n <= 0 -> e
        | n -> downto4 f (n-1) (f n e)

    let fac n = 
        match n with
        | 0 -> 1
        | n -> downto4 (fun x acc -> x*acc) n 1
    
    let range g n = downto4 (fun x a -> g x :: a) n []

    let rec double lst = 
        match lst with
        | [] -> []
        | x :: xs -> x*2 :: double xs
    
    let double_2 lst = List.map (fun x -> x*2) lst
    
    let rec stringLength lst =
        match lst with
        | [] -> []
        | x :: xs -> String.length x :: stringLength xs
        
    let stringLength_2 lst = List.map (fun x -> String.length x) lst
    
    let rec keepEven lst =
        match lst with
        | [] -> []
        | x :: xs when x % 2 = 0 -> x :: keepEven xs
        | x :: xs -> keepEven xs 
    
    let keepEven_2 lst = List.filter (fun x -> x % 2 = 0) lst
    
    let rec keepLengthGT5 lst = 
        match lst with
        | [] -> []
        | x :: xs when String.length x > 5 -> x :: keepLengthGT5 xs
        | x :: xs -> keepLengthGT5 xs
        
    let keepLengthGT5_2 lst = List.filter (fun x -> String.length x > 5) lst
    
    let rec sumPositive lst = 
        match lst with
        | [] -> 0
        | x :: xs when x >= 0 -> x + sumPositive xs
        | x :: xs -> sumPositive xs
    
    let rec sumPositive_2 lst = List.fold (fun acc x -> if x >= 0 then acc + x else acc) 0 lst
        
    let rec sumPositive_3 lst = lst |> List.filter (fun x -> x >= 0) |> List.fold (fun acc x -> x + acc) 0
    
    fun id x -> x
    let add5mul3_3 f x =  add5_2 f (x) |> mul3_2 id
    let rec mergeFuns _ = failwith "not implemented"
        
    let rec facFuns _ = failwith "not implemented"
        
    let fac_2 _ = failwith "not implemented"

    let removeOddIdx _= failwith "not implemented"
        
    
    let weird _ = failwith "not implemented"
    
   
    let insert _= failwith "not implemented"
                
    let rec permutations _ = failwith "not implemented"