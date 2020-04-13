module Main exposing (..)

import Browser
import Browser.Events as BE
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode as Decode
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { inputLetter : String
    , cursorPos : ( Float, Float )
    , notes : List ( String, ( Float, Float ) )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "J" ( 2.0, 2.0 ) []
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



--toDirection : String -> Direction
--toDirection string =
--case string of
--"H" ->
--Left
--"L" ->
--Right
--"J" ->
--Down
--"K" ->
--Up
--_ ->
--Other


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
    8.0


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
    let
        newNote =
            Tuple.pair note pos
    in
    case note of
        "W" ->
            newNote :: notes

        "H" ->
            newNote :: notes

        "Q" ->
            newNote :: notes

        "E" ->
            newNote :: notes

        "S" ->
            newNote :: notes

        _ ->
            notes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyInput key ->
            let
                dir =
                    getDirection key

                pos =
                    getNewCursorPos model.cursorPos dir

                notes =
                    addNotes model.notes (getNote key) pos
            in
            ( { model | inputLetter = dir, cursorPos = pos, notes = notes }
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


noteHeight : String
noteHeight =
    "14px"


noteWidth : String
noteWidth =
    "18px"


noteColor : String
noteColor =
    "white"


flagNoteDiv : Html Msg
flagNoteDiv =
    div
        [ style "position" "absolute"
        , style "top" "-25px"
        , style "left" "13px"
        , style "background-color" noteColor
        , style "width" "1px"
        , style "height" "30px"
        , style "transform" "skew(20deg)"
        , style "fontSize" "20px"
        ]
        []


wholeNoteDiv : Html Msg
wholeNoteDiv =
    div
        [ style "border" ("1px solid " ++ noteColor)
        , style "border-radius" "40%"
        , style "transform" "skew(-20deg)"
        , style "height" noteHeight
        , style "width" noteWidth
        ]
        []


halfNoteDiv : Html Msg
halfNoteDiv =
    div
        [ style "border" ("1px solid " ++ noteColor)
        , style "border-radius" "40%"
        , style "transform" "skew(-20deg)"
        , style "height" noteHeight
        , style "width" noteWidth
        ]
        [ flagNoteDiv ]


quarterNoteDiv : Html Msg
quarterNoteDiv =
    div
        [ style "border" ("1px solid " ++ noteColor)
        , style "border-radius" "40%"
        , style "transform" "skew(-20deg)"
        , style "height" noteHeight
        , style "width" noteWidth
        , style "background-color" noteColor
        ]
        [ flagNoteDiv ]


eigthNoteDiv : Html Msg
eigthNoteDiv =
    div
        [ style "border" ("1px solid " ++ noteColor)
        , style "border-radius" "40%"
        , style "transform" "skew(-20deg)"
        , style "height" noteHeight
        , style "width" noteWidth
        , style "background-color" noteColor
        ]
        [ flagNoteDiv ]


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
            eigthNoteDiv

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



--[ text name ]


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

        --, h1 [] [ text (Debug.toString model.cursorPos) ]
        --, h1 [] [ text (Debug.toString model.notes) ]
        , div [] (List.map noteDiv <| model.notes)
        , lineView
        , lineView
        , lineView
        , lineView
        , lineView
        ]


lineView : Html Msg
lineView =
    div lineContainerStyle
        [ div lineStyle []
        , div lineStyle []
        , div lineStyle []
        , div lineStyle []
        , div lineStyle []
        ]


lineStyle : List (Attribute msg)
lineStyle =
    [ style "width" "1500px"
    , style "borderBottom" "1px solid #444"
    , style "margin" "20px"
    , style "zIndex" "0"
    ]


lineContainerStyle : List (Attribute msg)
lineContainerStyle =
    [ style "padding" "15px"
    , style "marginTop" "20px"
    ]
