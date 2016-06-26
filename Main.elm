import Html exposing (..)
import String
import List
import Array

type Operator =  Add | Subtract | Multiply
type Token = TokenValue Float | TokenOperator String Int

type alias Config = List ConfigItem

type alias ConfigItem = 
    {
        operator : String
        , priority : Int
        , operation : Float -> Float -> Float
    }
operations : Config
operations = 
    [
        {
            operator = "+"
            , priority = 0
            , operation = (\n1 n2 -> n1 + n2)
        },
        {
            operator = "-"
            , priority = 0
            , operation = (\n1 n2 -> n1 - n2)
        },
        {
            operator = "*"
            , priority = 1
            , operation = (\n1 n2 -> n1 * n2)
        },
        {
            operator = "/"
            , priority = 1
            , operation = (\n1 n2 -> n1 / n2)
        }
    ] 

main = 
    div [] [
        div [] [text (calc "2 + 4 / 4")]
        , div [] [text (calc "2 + 2 + 3")]
        , div [] [text (calc "4 - 2")]
        , div [] [text (calc "4 / 2")]
    ]

calc : String -> String
calc calculation = 
    calculation
        |> lexer 
        |> calculator


calculator : List Token -> String        
calculator tokenList =
    let 
        tokenArray = Array.fromList tokenList
        
        priorityStep curr memo =
            calcPriority curr memo

        calcPriority priority list =
            list
                |> List.foldl (processStep priority) (Array.fromList [])
                |> Array.toList
    in
        priorities
            |> List.foldl priorityStep tokenList
            |> List.head
            |> Maybe.withDefault (TokenValue 0)
            |> tokenToInt
            |> toString    


processStep: Int -> Token -> Array.Array Token -> Array.Array Token
processStep priority curr memo =
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
            TokenOperator operator operatorPriority ->
                if operatorPriority == priority then
                    Array.push (TokenValue (operate n1 n2 operator)) (Array.slice 0 -2 memo)
                else 
                    Array.push curr memo
            TokenValue value ->
                Array.push curr memo 


tokenToInt : Token -> Float
tokenToInt token =
    case token of
        TokenValue value ->
            value
        TokenOperator operator priority ->
            0


lexer : String -> List Token
lexer string = 
    string
    |> String.split " "
    |> List.map tokenize


tokenize : String -> Token
tokenize string = 
    let
        token = operatorStringToToken string
        number = Result.withDefault 0 (String.toFloat string)  
    in
        Maybe.withDefault (TokenValue number) token


operatorStringToToken string =
    let 
        config = getOperationConfig string
    in
        case config of
            Maybe.Just config ->
                Maybe.Just (TokenOperator config.operator config.priority)
            Maybe.Nothing ->
                Maybe.Nothing


operate n1 n2 operator =
    let
        config = getOperationConfig operator
    in 
        case config of
            Maybe.Just config ->
                config.operation n1 n2
            Maybe.Nothing ->
                0


getOperationConfig operator =
    let 
        findConfig curr memo =
            if curr.operator == operator then
                Maybe.Just curr
            else
                memo
    in 
        List.foldl findConfig Maybe.Nothing operations

unique list =
    let 
        processItem curr memo =
            if (List.member curr memo) == False then
                curr :: memo
            else 
                memo 
    in
        List.foldl processItem [] list

priorities =
    operations    
        |> List.map (\config -> config.priority)
        |> unique
        |> List.sort
        |> List.reverse