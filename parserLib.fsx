open System;


type ParserResult<'a> =
| Success of 'a
| Failure of string

type Parser<'a> = Parser of (string -> ParserResult<'a * string>)


let run parser input = 
    let (Parser innerFn) = parser
    innerFn input

let pchar char =
    let innerFn input =
        if String.IsNullOrEmpty(input) then 
            Failure "No more input"
        else 
            let first = input.[0] 
            if first = char then
                let remaining = input.[1..]
                Success (char, remaining)
            else
                let msg = sprintf "Expecting '%c' got '%c'" char first
                Failure msg  
    Parser innerFn        


let andThen parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        | Failure err -> 
            Failure err  
        | Success (value1,remaining1) -> 
            let result2 =  run parser2 remaining1
            match result2 with 
            | Failure err ->
                Failure err 
            | Success (value2,remaining2) -> 
                let newValue = (value1,value2)
                Success (newValue,remaining2)
    Parser innerFn 

let ( .>>. ) = andThen


let orElse parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        | Success result -> 
            result1
        | Failure err -> 
            let result2 = run parser2 input
            result2 

    Parser innerFn 

let ( <|> ) = orElse

let choice listOfParsers = 
    List.reduce ( <|> ) listOfParsers 
