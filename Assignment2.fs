module Assignment2

    let rec downto1 n = 
        if n > 0 then
            n::downto1 (n-1)
        else
            []
    
    let rec downto2 n =
        match n with
        | 0 -> []
        | n -> n::downto2 (n-1) 

    let rec downto3 = 
        function
        | 0 -> []
        | n -> n::downto3(n-1)

    let rec removeOddIdx xs = 
        match xs with
        | [] -> []
        | [xs] -> [xs]
        | xs::_::rest -> xs:: removeOddIdx rest

    let rec combinePair xs =
        match xs with
        | [] -> []
        | [x] -> []
        | x::y::last -> (x,y)::combinePair last


    type complex = Complex of float*float
    
    let mkComplex a b  = Complex (a,b)

    let complexToPair (Complex (a,b)) = (a,b)

    //Solution for complex was found on Stack Overflow post below
    //https://stackoverflow.com/questions/60051250/f-specifying-a-custom-type-in-function-declaration-complex-number-to-pair

    let (|+|) (Complex (a,b)) (Complex (c,d)) = Complex (a+c, b+d) 

    let (|*|) (Complex (a,b)) (Complex (c,d)) = Complex (a*c-b*d,b*c+a*d)

    let (|-|) (Complex (a,b)) (Complex (c,d)) = (|+|) (Complex(a,b)) (Complex(-c,-d))

    let (|/|) (Complex (a,b)) (Complex (c,d)) = (|*|) (Complex(a,b)) (Complex(c/(c**2+d**2),-d/(c**2+d**2)))

    let explode1 (s:string) = s.ToCharArray() |> List.ofArray

    let rec explode2 (s:string) =
        match s with
        | "" -> []
        | x -> x.[0] :: explode2 (x.Remove(0,1))

    let rec implode (cs: char list) = 
        match cs with
        | [] -> ""
        | x -> x.[0].ToString() + implode x.[1 .. x.Length-1]

    let rec implodeRev (cs: char list) =
        match cs with
        | [] -> ""
        | x -> x.[x.Length-1].ToString() + implodeRev x.[0 .. x.Length-2] 

    let rec iterate (n:char list) = 
        match n with
        | [] -> []
        | n -> System.Char.ToUpper n.[0] :: iterate(n.[1 .. n.Length-1])

    let toUpper (s:string) = s |> explode1 |> iterate |> implode

    let rec ack (m,n)  = 
        match (m,n) with
        | (0,n) -> n + 1
        | (m,0) when m > 0 -> ack(m-1,1)
        | (m,n) -> ack (m-1,ack(m,n-1))
    
    let rec reverse lst = 
        match lst with
        | [] -> []
        | x :: xs -> reverse xs @ [x]
    
    let palindrome (str : string) = 
        let reversed = str |> explode1 |> reverse |> implode
        match reversed = str with
        | true -> true
        | false -> false

    let keepLetters _ = failwith "not implemented"
    let palindrome2 _ = failwith "not implemented"
    
    let palindrome3 _ = failwith "not implemented"