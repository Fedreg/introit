module Model exposing (Model, Note, Sequence)


type alias Model =
    { cursorPos : ( Float, Float )
    , notes : List Note
    }


type alias Sequence =
    { queuedNotes : List Note
    , playedNotes : List Note
    , tempo : Int
    }


type alias Note =
    { x : Float
    , y : Float
    , name : String
    , octave : Int
    , duration : Float
    , hz : Float
    }



-- type alias Note =
--     { x : Float
--     , y : Float
--     , name : String
--     , hz : Float
--     , octave : Int
--     , duration : Float
--     , wave : String
--     }
