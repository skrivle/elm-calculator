import Html exposing (Html, div, button, input, text)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Calculator.Calculator as Calculator


type Msg =
    InputChange String
    | Calculate

type alias Model =
    {
        input : String
        , output : String
    }
    

main : Program Never
main =
    App.beginnerProgram 
        {
            model = model
            , view = view
            , update = update
        } 


model : Model
model =
    {
        input = ""
        , output = ""
    }


view : Model -> Html Msg
view model =
    div [] [
        input [onInput InputChange, value model.input] [],
        button [onClick Calculate] [text "calculate"],
        div [] [
            text model.output
        ]
    ]


update : Msg -> Model -> Model
update msg model =
    case msg of 
        InputChange input ->
            {model | input = input}
        Calculate ->
            {model | output = calc model.input}


calc : String -> String 
calc = Calculator.calc
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