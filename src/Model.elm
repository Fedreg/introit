module Model exposing (Model)


type alias Model =
    { cursorPos : ( Float, Float )
    , notes : List ( List String, ( Float, Float ) )
    , mode : String
    , noteInput : List String
    }
