open System.IO

type TokenReader(s : string) =
    let mutable i = 0
    member tr.MoveNext() =
        i <- i + 1
    member tr.Peek() = 
        if i < s.Length then s.[i] else ' '
    member tr.Read() = 
        let ii = i
        i <- i + 1
        s.[ii]

type Expression =
    | Token of char
    | Binary of Expression * char * Expression

let parseExpression tokenParser (tokenReader : TokenReader) : Expression =
    let rec parseExpression1 (lhs : Expression, minPrecedence) =
        let mutable lhs = lhs
        let isBinaryOperator op = 
            ['='; '+'; '*'] |> List.contains op
        let isRightAssociative op = false
        let precedence op = 
            match op with
            | '=' -> 0
            | '+' -> 1
            | '*' -> 2
            | _ -> failwith (sprintf "not an operator: %c" op)
        let mutable lookahead = tokenReader.Peek()
        printfn "lookahead %A" lookahead
        while isBinaryOperator lookahead && precedence lookahead > minPrecedence do
            printfn "outer loop"
            let op = lookahead
            tokenReader.MoveNext()
            let mutable rhs = Token (tokenReader.Read())
            printfn "rhs %A" rhs
            lookahead <- tokenReader.Peek()
            printfn "lookahead2 %A" lookahead
            while (isBinaryOperator lookahead && precedence lookahead > precedence op)
               || (isRightAssociative lookahead && precedence lookahead = precedence op) do
                printfn "innerLoop"
                rhs <- parseExpression1 (rhs, precedence lookahead)
                lookahead <- tokenReader.Peek()
            lhs <- Binary (lhs, op, rhs)
        lhs
    let token = (Token (tokenReader.Read()))
    parseExpression1 (token, 0)


let input = "2+3+3"
let reader = TokenReader(input)
parseExpression reader
