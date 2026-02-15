module Assignment1
    
    let sqr x = x * x

    let pow x n= System.Math.Pow(x,n)
    
    let rec fib =
        function
        | 0 -> 0
        | 1 -> 1
        | n -> fib (n-1) + fib (n-2)

    let rec sum = 
        function
        | 0 -> 0
        | n -> n + sum (n - 1)

    let dup (s :string) = s + s

    let rec dupn (s: string) =
        function
        | 0 -> ""
        | n -> s + dupn s (n-1)

    let rec readInt() = 
        let readFromConsole = System.Console.ReadLine().Trim()
        let tryParseInt (str: string) = System.Int32.TryParse str
        let input = tryParseInt readFromConsole

        match input with
        | (true, result) -> result
        | (false, result) -> 
            printfn "%d is not an integer" result 
            readInt()

    let timediff (hh1, mm1) (hh2, mm2) =
        60*(hh2-hh1) + (mm2-mm1)

    let minutes (hh,mm) = 
        timediff (00,00) (hh,mm)

    let rec bin (n, k) = 
        match (n, k) with
        | (n, 0) -> 1
        | (n, k) when n = k -> 1
        | (n, k) -> bin (n - 1, k-1) + bin (n-1, k)
    

    (*let curry _ = failwith "not implemented"

    let uncurry _ = failwith "not implemented"
    *)
    let curry f a b = f(a, b)

    let uncurry f (a,b) =  f a b
    
    (*
    Definition of curry:
        When a function takes multiple variables, currying then creates a
        sequence of functions which only takes one argument

    Definition of uncurry:
        Uncurry is the opposite of curry meaning it takes a sequence of single-argument
        functions and turns it into a single function which takes all arguments at once
    *)

