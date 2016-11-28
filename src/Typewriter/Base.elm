module Typewriter.Base
    exposing
        ( Model
        , Msg(..)
        , init
        , initDemo
        , update
        , view
        , subscriptions
        , choice
        , choiceComplete
        , keycode
        )

import Html exposing (text, div, ul)
import Html
import String
import Keyboard
import Char


main : Program Never Model Msg
main =
    Html.program
        { init = initDemo
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { choices : List String
    , input : String
    , lastKey : Char.KeyCode
    }


init : List String -> ( Model, Cmd Msg )
init choices =
    { choices = choices
    , input = ""
    , lastKey = 0
    }
        ! []


initDemo : ( Model, Cmd Msg )
initDemo =
    { choices = [ "Arm ", "Armchair ", "Racist ", "Dunderhead " ]
    , input = ""
    , lastKey = 0
    }
        ! []



-- UPDATE


type Msg
    = KeyPress Char.KeyCode



-- returns Nothing, or phrase if word selected


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress key ->
            let
                newInput =
                    parseInput model key
            in
                -- only update input buffer if it's a valid prefix
                if validate model.choices newInput then
                    ( { model | input = newInput, lastKey = key }, Cmd.none )
                else
                    ( { model | lastKey = key }, Cmd.none )


parseInput : Model -> Char.KeyCode -> String
parseInput model key =
    -- recognizes backspace, space, and alphanumeric plus dash
    if key == keycode.backspace then
        if model.lastKey == keycode.backspace then
            ""
        else
            String.slice 0 -1 model.input
    else if key == keycode.dash then
        model.input ++ "-"
    else if (isKeyLetter key) || (key == keycode.space) then
        model.input ++ (Char.fromCode key |> String.fromChar)
    else
        model.input


startsWith : String -> String -> Bool
startsWith a b =
    String.startsWith (String.toLower a) (String.toLower b)


validate : List String -> String -> Bool
validate choices input =
    List.any (startsWith input) choices


isKeyLetter : Int -> Bool
isKeyLetter code =
    let
        key =
            Char.fromCode code
    in
        (Char.isUpper key) || (Char.isLower key) || (key == '-')


choice : Model -> Maybe String
choice model =
    if model.input == "" then
        Nothing
    else
        model.choices
            |> List.filter (startsWith model.input)
            |> List.sortBy String.length
            |> List.head


{-| Either we have an exact match, or special case where last character is
space and up that gives exactly one match.
-}
choiceComplete : Model -> Bool
choiceComplete model =
    let
        hits =
            model.choices
                |> List.filter (startsWith model.input)

        specialCase =
            (List.length hits == 1) && (model.lastKey == keycode.space)

        specialCase2 =
            (model.input == "")
                && (model.lastKey == keycode.space)
                && (List.member "" model.choices)
    in
        case choice model of
            Nothing ->
                specialCase2

            Just s ->
                if specialCase then
                    True
                else
                    (String.toLower s) == (String.toLower model.input)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyPress
        ]


type alias Keycode =
    { escape : Int
    , shift : Int
    , space : Int
    , enter : Int
    , backspace : Int
    , dash : Int
    }


keycode : Keycode
keycode =
    { enter = 13, shift = 16, space = 32, escape = 27, backspace = 8, dash = 189 }



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        x =
            choice model |> Maybe.withDefault "[none]"

        y =
            if choiceComplete model then
                "choice complete"
            else
                "incomplete"
    in
        div []
            [ div [] [ text model.input ]
            , div [] [ text x ]
            , div [] [ text y ]
            , div [] (List.map (\s -> ul [] [ text s ]) model.choices)
            ]
