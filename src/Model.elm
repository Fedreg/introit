module Model exposing (Model, Note, Sequence)


type alias Model =
    { cursorPos : ( Float, Float )
    , notes : List Note
    , timeSignature : ( Int, Int )
    , measureCount : Int
    }


type alias Sequence =
    { queuedNotes : List Note
    , playedNotes : List Note
    , tempo : Int
    }


-- TODO
-- Keep track of measure and beat in that measure when I move cursor
-- Then I can build a note id that is M13BQ, which would be: Measure 1, Beat 3, Note B, Duration Q
-- Also add an id to each measure, can just be 1, 2 etc.
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
