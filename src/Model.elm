module Model exposing (Model, Note)


type alias Model =
    { cursorPos : ( Float, Float )
    , notes : List Note
    }


type alias Note =
    { x : Float
    , y : Float
    , name : String
    , octave : Int
    , duration : String
    }
