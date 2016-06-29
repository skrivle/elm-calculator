module Calculator.Lexer exposing (Token(..), lexer, tokenToInt)

import String
import Calculator.Config exposing (Config, ConfigItem)


type alias TokenValue = Float

type Token = 
    TokenValue Float 
    | TokenOperator 
        String Int (Float -> Float -> Float)


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


operatorStringToToken : Config -> String -> Maybe Token
operatorStringToToken config string =
    let 
        configItem = operationConfig config string
    in
        case configItem of
            Maybe.Just config ->
                Maybe.Just (TokenOperator config.operator config.priority config.operation)
            Maybe.Nothing ->
                Maybe.Nothing


operationConfig : Config -> String -> Maybe ConfigItem  
operationConfig config operator =
    config
        |> List.filter (\item -> item.operator == operator)
        |> List.head


tokenToInt : Token -> Float
tokenToInt token =
    case token of
        TokenValue value ->
            value
        TokenOperator operator priority operation ->
            0