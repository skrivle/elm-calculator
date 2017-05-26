module Calculator.Config exposing (Config, ConfigItem)


type alias Config =
    List ConfigItem


type alias ConfigItem =
    { operator : String
    , priority : Int
    , operation : Float -> Float -> Float
    }
