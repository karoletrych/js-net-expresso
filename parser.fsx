#load "parserLib.fsx"
open ParserLib

let i0 = "a"
let i1 = "a: 1+2"

let a = pChar 'a'

run a "aaaA"


let pRule = pIdentifier .>> pChar ':' >>. pExpression