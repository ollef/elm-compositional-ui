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
        Kui.column
            [ Kui.row
                [ Kui.padLeftPct 10 <|
                    Kui.padLeft 10 <|
                        Kui.padRight 10 <|
                            Kui.alignRight <|
                                Kui.background (Kui.rgba 1 0 0 0.5) <|
                                    Kui.text "Hello, compositional world!"
                , Kui.text "Hello2"
                    |> Kui.padRight 20
                    |> Kui.alignRight
                , Kui.center <| Kui.text "Another item"
                , Kui.text "Third item"
                    |> Kui.border
                        { width = Kui.Pixels 3
                        , color = Kui.rgb 0 0.5 0.5
                        , radius = Kui.Pixels 2
                        }
                    |> Kui.padTop 20
                , Kui.column
                    [ Kui.text "Column"
                    , Kui.text "With an item"
                    , Kui.text "3"
                    , Kui.text "4"
                    ]
                ]
            , let
                border =
                    Kui.border { width = Kui.Pixels 3, color = Kui.rgb 0 0 0, radius = Kui.Pixels 2 }
              in
              Kui.column
                [ Kui.row <|
                    List.map (border << Kui.alignTop)
                        [ Kui.alignLeft <| Kui.text "top left"
                        , Kui.centerX <| Kui.text "top"
                        , Kui.alignRight <| Kui.text "top right"
                        ]
                , Kui.row <|
                    List.map (border << Kui.centerY)
                        [ Kui.alignLeft <| Kui.text "center left"
                        , Kui.centerX <| Kui.text "center"
                        , Kui.alignRight <| Kui.text "center right"
                        ]
                , Kui.row <|
                    List.map (border << Kui.alignBottom)
                        [ Kui.alignLeft <| Kui.text "bottom left"
                        , Kui.centerX <| Kui.text "bottom"
                        , Kui.alignRight <| Kui.text "bottom right"
                        ]
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
