module Model =
    type Operator =
    | Plus
    | Minus
    | Multiply
    | Divide
    | Equals

    type Expression = 
    | Literal of string
    | Unary of Operator * Expression
    | Binary of Expression * Operator * Expression
    | Ternary of Expression * Expression * Expression
    | OtherLabel of string 
    | FunctionCall of string * Expression list

    type Definition = {
        Label: string
        Expression: Expression
    }

