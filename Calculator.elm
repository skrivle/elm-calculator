module Calculator exposing (calc, Config)

import String
import List
import Array

import Utils

type Token = 
    TokenValue Float 
    | TokenOperator String Int (Float -> Float -> Float)

type alias Config = List ConfigItem

type alias ConfigItem = 
    {
        operator : String
        , priority : Int
        , operation : Float -> Float -> Float
    }

calc : Config -> String -> String        
calc config string =
    let 
        tokenList = (lexer config) string
        calcPriority priority list =
            list
                |> List.foldl (processStep priority) (Array.fromList [])
                |> Array.toList
    in
        (priorities config) 
            |> List.foldl calcPriority tokenList
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
            TokenOperator operator operatorPriority operation ->
                if operatorPriority == priority then
                    Array.push (TokenValue (operation n1 n2)) (Array.slice 0 -2 memo)
                else 
                    Array.push curr memo
            TokenValue value ->
                Array.push curr memo 


tokenToInt : Token -> Float
tokenToInt token =
    case token of
        TokenValue value ->
            value
        TokenOperator operator priority operation ->
            0


lexer : Config -> String -> List Token
lexer config string = 
    string
    |> String.split " "
    |> List.map (tokenize config)


tokenize : Config -> String -> Token
tokenize config string = 
    let
        token = operatorStringToToken config string
        number = Result.withDefault 0 (String.toFloat string)  
    in
        Maybe.withDefault (TokenValue number) token


operatorStringToToken config string =
    let 
        configItem = operationConfig config string
    in
        case configItem of
            Maybe.Just config ->
                Maybe.Just (TokenOperator config.operator config.priority config.operation)
            Maybe.Nothing ->
                Maybe.Nothing


operationConfig config operator =
    let 
        findConfig curr memo =
            if curr.operator == operator then
                Maybe.Just curr
            else
                memo
    in 
        List.foldl findConfig Maybe.Nothing config

priorities config =
    config    
        |> List.map (\config -> config.priority)
        |> Utils.unique
        |> List.sort
        |> List.reverse