open System;


type ParserResult<'a> =
| Success of 'a
| Failure of string

type Parser<'a> = Parser of (string -> ParserResult<'a * string>)

// let parser x : Parser =

let pChar char =
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



let run parser input = 
    let (Parser innerFn) = parser
    innerFn input