module Model exposing (Model, Note, Sequence)


type alias Model =
    { cursorPos : ( Float, Float )
    , notes : List Note
    , timeSignature : ( Int, Int )
    , beatCount : Float
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
    , durationType : String
    , hz : Float
    }
