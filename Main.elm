import Html exposing (..)
import String
import List
import Array

type Operator =  Add | Subtract | Multiply
type Token = TokenValue Int | TokenOperator Operator


-- operations = 
--     [
--         {
--             operator = "+"
--             , priority = 0
--             , operation = (\n1 n2 -> n1 + n2)
--         },
--         {
--             operator = "-"
--             , priority = 0
--             , operation = (\n1 n2 -> n1 - n2)
--         },
--         {
--             operator = "*"
--             , priority = 1
--             , operation = (\n1 n2 -> n1 * n2)
--         }
--     ] 

main = text (calc "2 * 2")

calc : String -> String
calc calculation = 
    calculation
        |> lexer 
        |> calculator

calculator : List Token -> String        
calculator tokenList =
    tokenList 
        |> List.foldl processStep (Array.fromList [])
        |> Array.get 0
        |> Maybe.withDefault (TokenValue 0)
        |> tokenToInt
        |> toString 

processStep: Token -> Array.Array Token -> Array.Array Token
processStep curr memo =
    let 
        lastIndex = (Array.length memo) - 1
        lastItem = (Array.get lastIndex memo)
            |> Maybe.withDefault (TokenValue 0)
            
        n1 = Array.get (lastIndex - 1) memo
            |> Maybe.withDefault (TokenValue 0)
            |> tokenToInt
        n2 = tokenToInt curr
        operator = lastItem
    in
        case lastItem of
            TokenOperator operator ->
                Array.fromList [(TokenValue (operate n1 n2 operator))]
            TokenValue value ->
                Array.push curr memo 


tokenToInt : Token -> Int
tokenToInt token =
    case token of
        TokenValue value ->
            value
        TokenOperator operator ->
            0

lexer : String -> List Token
lexer string = 
    string
    |> String.split " "
    |> List.map tokenize


tokenize : String -> Token
tokenize string = 
    let
        token = stringToToken string
        number = Result.withDefault 0 (String.toInt string)  
    in
        Maybe.withDefault (TokenValue number) token
            
            

stringToToken : String -> Maybe Token
stringToToken string =
    case string of
        "+" ->
            Maybe.Just (TokenOperator Add)
        "-" ->
            Maybe.Just (TokenOperator Subtract)
        "*" ->
            Maybe.Just (TokenOperator Multiply)
        _ -> 
            Maybe.Nothing




operate n1 n2 operator = 
    case operator of
        Add -> 
            (\n1 n2 -> n1 + n2) n1 n2
        Subtract -> 
            (\n1 n2 -> n1 - n2) n1 n2
        Multiply ->
            (\n1 n2 -> n1 * n2) n1 n2

