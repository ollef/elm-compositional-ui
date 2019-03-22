module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Kui



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Kui.layout [] <|
        Kui.row
            [ Kui.background (Kui.rgba 1 0 0 0.5) <| Kui.alignRight <| Kui.text "Hello, compositional world!"
            , Kui.text "Another item"
            , Kui.text "Third item"
            , Kui.column
                [ Kui.text "Column"
                , Kui.text "With an item"
                ]
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
