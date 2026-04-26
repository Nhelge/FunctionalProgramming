module Interpreter.Parser

    open Interpreter.Language

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use if performance gets bad
    

    let pif       : Parser<string> = pstring "if"
    let pelse     : Parser<string> = pstring "else"
    let palloc    : Parser<string> = pstring "alloc"
    let pfree     : Parser<string> = pstring "free"
    let pwhile    : Parser<string> = pstring "while"
    let pdo       : Parser<string> = pstring "do"
    let pdeclare  : Parser<string> = pstring "declare"
    let ptrue     : Parser<string> = pstring "true"
    let pfalse    : Parser<string> = pstring "false"
    let pprint    : Parser<string> = pstring "print"
    let prandom   : Parser<string> = pstring "random"
    let pread     : Parser<string> = pstring "read"
    let pfunction : Parser<string> = pstring "function"
    let pret      : Parser<string> = pstring "ret"

    let pwhitespaceChar = satisfy (fun c -> System.Char.IsWhiteSpace c)
    let pletter = satisfy (fun c -> System.Char.IsLetter c)
    let palphanumeric   = satisfy (fun c -> System.Char.IsLetterOrDigit c)

    let spaces = many (satisfy (fun c -> System.Char.IsWhiteSpace c))
    let spaces1 = many1 (satisfy (fun c -> System.Char.IsWhiteSpace c))

    let (.>*>.) a b = a .>> spaces .>>. b
    let (.>*>) a b  = a .>> spaces .>> b
    let (>*>.) a b  = a >>. spaces >>. b

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let parenthesise2 p = pchar '{' >*>. p .>*> pchar '}'
    let parseString= 
        pchar '"' >>. many (satisfy (fun c -> c <> '"')) .>> pchar '"' |>> 
            fun chars -> 
                let str = new string [|for c in chars -> c|]
                str.Replace("\\n", "\n").Replace("\\t", "\t")

    let aux (c, cs) = c :: cs |> List.toArray |> System.String

    let pid = pletter <|> pchar '_' .>>. many (palphanumeric <|> pchar '_') |>> aux
    let unop op a = op >*>. a
    let binop op a b = a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aexpr>()
    let ProdParse, pref = createParserForwardedToRef<aexpr>()
    let AtomParse, aref = createParserForwardedToRef<aexpr>()
    let AParse, apref = createParserForwardedToRef<aexpr>()

    let NegParse = pchar '-' >>. AtomParse |>> fun a -> Mul (Num -1, a)
    let VarParse = pid |>> Var
    let RandomParse = prandom |>> fun _ -> Random
    let ReadParse = pread |>> fun _ -> Read
    let MemReadParse = pchar '[' >>. AParse .>> pchar ']' |>> fun x -> MemRead x
    let SubParse = binop (pchar '-') ProdParse TermParse |>> fun (x, y) -> x .-. y
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> fun (x, y) -> x .%. y

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> Num <?> "Int"
    let ParParse = parenthesise AParse
    do aref := choice [NegParse; ReadParse; RandomParse; MemReadParse; VarParse; NParse; ParParse]

    let B1, b1ref = createParserForwardedToRef<bexpr>()
    let B2, b2ref = createParserForwardedToRef<bexpr>()

    let TrueParse = ptrue |>> fun _ -> TT
    let FalseParse = pfalse |>> fun _ -> Not TT
    let EqParse = binop (pchar '=') ProdParse TermParse |>> Eq <?> "Eq"
    let LtParse = binop (pchar '<') ProdParse TermParse |>> Lt <?> "Lt"
    let GtParse = binop (pchar '>') ProdParse TermParse |>> fun (x, y) -> x .>. y
    let LeParse = binop (pstring "<=") ProdParse TermParse |>> fun (x, y) -> x .<=. y
    let GeParse = binop (pstring ">=") ProdParse TermParse |>> fun (x, y) -> x .>=. y
    let NeParse = binop (pstring "<>") ProdParse TermParse |>> fun (x, y) -> x .<>. y
    let NotParse = pchar '~' >>. B2 |>> fun x -> Not x
    let AndParse = binop (pstring "/\\") B2 B1 |>> Conj <?> "Conj"
    let OrParse = binop (pstring "\\/") B2 B1 |>> fun (x, y) -> x .||. y
    let BParParse = parenthesise B1

    do b1ref := choice [AndParse; OrParse; B2]
    do b2ref := choice [NotParse; TrueParse; FalseParse; LeParse; GeParse; NeParse; EqParse; LtParse; GtParse; BParParse]

    let paexpr = AParse

    let pbexpr = B1
    let CondParse = pbexpr .>> pchar '?' .>>. paexpr .>> pchar ':' .>>. paexpr |>> fun ((b, t), f) -> Cond (b, t, f)
    do apref := choice [CondParse; TermParse]

    let S1, s1ref = createParserForwardedToRef<stmnt>()
    let S2, s2ref = createParserForwardedToRef<stmnt>()

    let AssignParse = pid .>*> pstring ":=" .>*>. paexpr |>> Assign
    let MemParse = pchar '[' >>. paexpr .>> pchar ']' .>*> pstring ":=" .>*>. paexpr |>> MemWrite
    let SeqParse = S2 .>> pchar ';' .>>. S1 |>> Seq
    let DecParse = pdeclare >>. spaces1 >>. pid |>> Declare
    let IfElseParse = pif >*>. parenthesise pbexpr .>*>. parenthesise2 S1 .>*> pelse .>*>. parenthesise2 S1 |>> fun ((b, s1),s2) -> If (b, s1, s2)
    let IfSkipParse = pif >*>. parenthesise pbexpr .>*>. parenthesise2 S1 |>> fun (x, y) -> If(x, y, Skip)
    let IfParse = IfElseParse <|> IfSkipParse
    let WhileParse = pwhile >*>. parenthesise pbexpr .>*>. parenthesise2 S1 |>> While
    let AlParse = palloc >*>. parenthesise (pid .>> pchar ',' .>>. paexpr) |>> Alloc
    let FrParse = pfree >*>. parenthesise (paexpr .>> pchar ',' .>>. paexpr) |>> Free
    let PrParse = pprint >*>. parenthesise (parseString .>> many (pchar ',') .>>. many paexpr) |>> fun (x, y) -> Print (y, x)

    do s1ref := choice [SeqParse; S2]
    do s2ref := choice [AssignParse; DecParse; IfParse; WhileParse; AlParse; FrParse; PrParse; MemParse]

    let pstmnt = S1
    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  
