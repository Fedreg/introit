module Main exposing (..)

import Browser
import Browser.Events as BE
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode as Decode
import Model exposing (Model)
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model ( 20.0, 20.0 ) []
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyInput String


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


type NoteRhythm
    = Whole
    | Half
    | Quarter
    | Eigth
    | Sixteenth


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


getDirection : String -> String
getDirection key =
    case key of
        "j" ->
            "Down"

        "k" ->
            "Up"

        "h" ->
            "Left"

        "l" ->
            "Right"

        _ ->
            ""


getNote : String -> String
getNote key =
    case key of
        "w" ->
            "W"

        "r" ->
            "H"

        "q" ->
            "Q"

        "e" ->
            "E"

        "s" ->
            "S"

        _ ->
            ""


moveConst : Float
moveConst =
    10.0


getNewCursorPos : ( Float, Float ) -> String -> ( Float, Float )
getNewCursorPos currentPos dir =
    let
        x =
            Tuple.first currentPos

        y =
            Tuple.second currentPos
    in
    case dir of
        "Left" ->
            ( x - moveConst, y )

        "Right" ->
            ( x + moveConst, y )

        "Up" ->
            ( x, y + moveConst )

        "Down" ->
            ( x, y - moveConst )

        _ ->
            ( x, y )


addNotes : List ( String, ( Float, Float ) ) -> String -> ( Float, Float ) -> List ( String, ( Float, Float ) )
addNotes notes note pos =
    case note of
        "W" ->
            Tuple.pair note pos :: notes

        "H" ->
            Tuple.pair note pos :: notes

        "Q" ->
            Tuple.pair note pos :: notes

        "E" ->
            Tuple.pair note pos :: notes

        "S" ->
            Tuple.pair note pos :: notes

        _ ->
            notes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyInput key ->
            let
                dir =
                    getDirection key

                note =
                    getNote key

                pos =
                    getNewCursorPos model.cursorPos dir

                notes =
                    addNotes model.notes note pos
            in
            ( { model | cursorPos = pos, notes = notes }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BE.onKeyPress (Decode.map KeyInput keyDecoder)
        ]



-- VIEW


cursor : ( Float, Float ) -> Html Msg
cursor pos =
    let
        x =
            Tuple.first pos

        y =
            negate (Tuple.second pos)
    in
    div
        [ style "background-color" "red"
        , style "height" "20px"
        , style "width" "10px"
        , style "position" "fixed"
        , style "top" (Debug.toString y ++ "px")
        , style "left" (Debug.toString x ++ "px")
        ]
        []


defaultNoteForMaybe =
    Tuple.pair "W" (Tuple.pair 0 0)


noteHeight : Int
noteHeight =
    19


noteWidth : Int
noteWidth =
    20


staffLineHeight : Int
staffLineHeight =
    20


noteColor : String
noteColor =
    "white"


baseNoteDiv : String -> Float -> Html Msg
baseNoteDiv color widthMult =
    div
        [ style "border" "1px solid #fff"
        , style "border-radius" "5px"
        , style "height" (String.fromInt noteHeight ++ "px")
        , style "width" (Debug.toString (toFloat noteWidth * widthMult) ++ "px")
        , style "background-color" color
        , style "zIndex" "10"
        ]
        []


wholeNoteDiv : Html Msg
wholeNoteDiv =
    baseNoteDiv "" 4.0


halfNoteDiv : Html Msg
halfNoteDiv =
    baseNoteDiv "#444" 2.0


quarterNoteDiv : Html Msg
quarterNoteDiv =
    baseNoteDiv "#888" 1.0


eigthNoteDiv : Html Msg
eigthNoteDiv =
    baseNoteDiv "#bbb" 0.5


sixteenthNoteDiv : Html Msg
sixteenthNoteDiv =
    baseNoteDiv "#fff" 0.25


noteDivChooser : String -> Html Msg
noteDivChooser name =
    case name of
        "W" ->
            wholeNoteDiv

        "H" ->
            halfNoteDiv

        "Q" ->
            quarterNoteDiv

        "E" ->
            eigthNoteDiv

        "S" ->
            sixteenthNoteDiv

        _ ->
            div [] []


noteDiv : ( String, ( Float, Float ) ) -> Html Msg
noteDiv note =
    let
        name =
            Tuple.first note

        pos =
            Tuple.second note

        x =
            Tuple.first pos

        y =
            negate (Tuple.second pos)
    in
    div
        [ style "height" "20px"
        , style "width" "20px"
        , style "position" "fixed"
        , style "top" (Debug.toString y ++ "px")
        , style "left" (Debug.toString x ++ "px")
        ]
        [ noteDivChooser name ]


view : Model -> Html Msg
view model =
    div
        [ style "background-color" "#111"
        , style "height" "1000px"
        , style "color" "white"
        , style "marginTop" "-25px"
        , style "paddingTop" "100px"
        ]
        [ cursor model.cursorPos
        , h1 [] [ text (Debug.toString model.cursorPos) ]

        -- , h1 [] [ text (Debug.toString model.notes) ]
        , staff
        , div [] (List.map noteDiv <| model.notes)
        ]


staff : Html Msg
staff =
    div staffContainerStyle
        [ div (staffStyle 20) []
        , div (staffStyle 39) []
        , div (staffStyle 58) []
        , div (staffStyle 77) []
        , div (staffStyle 96) []
        ]


staffStyle : Int -> List (Attribute msg)
staffStyle top =
    [ style "position" "relative"
    , style "top" (Debug.toString top ++ "px")
    , style "width" "1500px"
    , style "borderBottom" "1px solid #444"
    ]


staffContainerStyle : List (Attribute msg)
staffContainerStyle =
    [ --style "padding" "15px"
      style "marginTop" "20px"
    , style "position" "absolute"
    , style "top" "100px"
    , style "left" "50px"
    , style "box-sizing" "border-box"
    ]
