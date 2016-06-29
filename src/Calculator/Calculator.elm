module Calculator.Calculator exposing (calc)

import List
import Array
import Utils
import Calculator.Config exposing (Config)
import Calculator.Lexer exposing (Token(..), lexer, tokenToInt)


calc : Config -> String -> String        
calc config string =
    let 
        tokenList = 
            (lexer config) string
            
        calcPriority priority list =
            list
                |> List.foldl (processToken priority) (Array.fromList [])
                |> Array.toList
    in
        (priorities config) 
            |> List.foldl calcPriority tokenList
            |> List.head
            |> Maybe.withDefault (TokenValue 0)
            |> tokenToInt
            |> toString    


processToken: Int -> Token -> Array.Array Token -> Array.Array Token
processToken priority token list =
    let 
        lastIndex = 
            (Array.length list) - 1
        lastItem = 
            (Array.get lastIndex list)
                |> Maybe.withDefault (TokenValue 0)
            
        n1 = 
            Array.get (lastIndex - 1) list
                |> Maybe.withDefault (TokenValue 0)
                |> tokenToInt

        n2 = 
            tokenToInt token

        operator = 
            lastItem
    in
        case lastItem of
            TokenOperator operator operatorPriority operation ->
                if operatorPriority == priority then
                    Array.push (TokenValue (operation n1 n2)) (Array.slice 0 -2 list)
                else 
                    Array.push token list
            TokenValue value ->
                Array.push token list 


priorities : Config -> List Int
priorities config =
    config    
        |> List.map (\config -> config.priority)
        |> Utils.unique
        |> List.sort
        |> List.reverse