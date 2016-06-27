import Html exposing (..)
import Calculator

config : Calculator.Config
config = 
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

calc = Calculator.calc config

main = 
    div [] [
        div [] [text (calc "2 + 4 / 4")]
        , div [] [text (calc "2 + 2 + 3")]
        , div [] [text (calc "4 - 2")]
        , div [] [text (calc "4 / 2")]
    ]

