module Kui exposing
    ( Border
    , Color
    , Direction(..)
    , Element
    , HAlignment(..)
    , Length(..)
    , Padding
    , VAlignment(..)
    , Wrap(..)
    , addHAlignment
    , addLength
    , addPadding
    , addVAlignment
    , alignBottom
    , alignLeft
    , alignRight
    , alignTop
    , attribute
    , attributes
    , background
    , border
    , center
    , centerX
    , centerY
    , column
    , isZeroLength
    , layout
    , noPadding
    , none
    , pad
    , padBottom
    , padBottomPct
    , padLeft
    , padLeftPct
    , padRight
    , padRightPct
    , padTop
    , padTopPct
    , renderLength
    , rgb
    , rgb255
    , rgba
    , rgba255
    , row
    , stack
    , text
    , wrappedRow
    , zeroLength
    )

import Hex
import Html exposing (Html)
import Html.Attributes as Html



-------------------------------------------------------------------------------
-- Color


type alias Color =
    { red : Float, green : Float, blue : Float, alpha : Float }


rgba : Float -> Float -> Float -> Float -> Color
rgba =
    Color


rgb : Float -> Float -> Float -> Color
rgb r g b =
    rgba r g b 1


rgb255 : Int -> Int -> Int -> Color
rgb255 r g b =
    rgba255 r g b 1


rgba255 : Int -> Int -> Int -> Float -> Color
rgba255 r g b =
    Color (toFloat r / 255) (toFloat g / 255) (toFloat b / 255)


renderColor : Color -> String
renderColor { red, green, blue, alpha } =
    let
        go x =
            String.padLeft 2 '0' <| Hex.toString <| round <| x * 255
    in
    "#"
        ++ String.concat
            (List.map go <|
                if alpha == 1 then
                    [ red, green, blue ]

                else
                    [ red, green, blue, alpha ]
            )



-------------------------------------------------------------------------------
-- Padding


type Length
    = Pixels Float
    | Percent Float
    | Add Length Length


zeroLength : Length
zeroLength =
    Pixels 0


isZeroLength : Length -> Bool
isZeroLength l =
    case l of
        Pixels p ->
            p == 0

        Percent p ->
            p == 0

        Add _ _ ->
            False


addLength : Length -> Length -> Length
addLength l1 l2 =
    case ( l1, l2 ) of
        ( Pixels p1, Pixels p2 ) ->
            Pixels (p1 + p2)

        ( Percent p1, Percent p2 ) ->
            Percent (p1 + p2)

        _ ->
            if isZeroLength l1 then
                l2

            else if isZeroLength l2 then
                l1

            else
                Add l1 l2


renderLength : Length -> String
renderLength =
    let
        go calculating l =
            case l of
                Pixels p ->
                    String.fromFloat p ++ "px"

                Percent p ->
                    String.fromFloat p ++ "%"

                Add l1 l2 ->
                    let
                        inner =
                            go True l1 ++ " + " ++ go True l2
                    in
                    if calculating then
                        inner

                    else
                        "calc(" ++ inner ++ ")"
    in
    go False


type alias Padding =
    { top : Length
    , right : Length
    , bottom : Length
    , left : Length
    }


noPadding : Padding
noPadding =
    { top = zeroLength, right = zeroLength, bottom = zeroLength, left = zeroLength }


addPadding : Padding -> Padding -> Padding
addPadding p1 p2 =
    { top = addLength p1.top p2.top
    , right = addLength p1.right p2.right
    , bottom = addLength p1.bottom p2.bottom
    , left = addLength p1.left p2.left
    }


renderPadding : Padding -> List (Html.Attribute msg)
renderPadding padding =
    if padding == noPadding then
        []

    else
        [ Html.style "padding" <|
            renderLength padding.top
                ++ " "
                ++ renderLength padding.right
                ++ " "
                ++ renderLength padding.bottom
                ++ " "
                ++ renderLength padding.left
        ]



-------------------------------------------------------------------------------
-- Alignment


type HAlignment
    = Left
    | HCenter
    | Right
    | HStretch


addHAlignment : HAlignment -> HAlignment -> HAlignment
addHAlignment h1 h2 =
    case h1 of
        HStretch ->
            h2

        _ ->
            h1


renderHAlignment : Direction -> HAlignment -> List (Html.Attribute msg)
renderHAlignment dir halign =
    case dir of
        Row ->
            case halign of
                Left ->
                    [ Html.style "margin-right" "auto" ]

                HCenter ->
                    [ Html.style "margin-left" "auto", Html.style "margin-right" "auto" ]

                Right ->
                    [ Html.style "margin-left" "auto" ]

                HStretch ->
                    [ Html.style "flex-grow" "1" ]

        ReverseRow ->
            case halign of
                Left ->
                    [ Html.style "margin-right" "auto" ]

                HCenter ->
                    [ Html.style "margin-left" "auto", Html.style "margin-right" "auto" ]

                Right ->
                    [ Html.style "margin-left" "auto" ]

                HStretch ->
                    [ Html.style "flex-grow" "1" ]

        Column ->
            case halign of
                Left ->
                    [ Html.style "align-self" "flex-start" ]

                HCenter ->
                    [ Html.style "align-self" "center" ]

                Right ->
                    [ Html.style "align-self" "flex-end" ]

                HStretch ->
                    []

        ReverseColumn ->
            case halign of
                Left ->
                    [ Html.style "align-self" "flex-start" ]

                HCenter ->
                    [ Html.style "align-self" "center" ]

                Right ->
                    [ Html.style "align-self" "flex-end" ]

                HStretch ->
                    []


type VAlignment
    = Top
    | VCenter
    | Bottom
    | VStretch


addVAlignment : VAlignment -> VAlignment -> VAlignment
addVAlignment v1 v2 =
    case v1 of
        VStretch ->
            v2

        _ ->
            v1


renderVAlignment : Direction -> VAlignment -> List (Html.Attribute msg)
renderVAlignment dir valign =
    case dir of
        Row ->
            case valign of
                Top ->
                    [ Html.style "align-self" "flex-start" ]

                VCenter ->
                    [ Html.style "align-self" "center" ]

                Bottom ->
                    [ Html.style "align-self" "flex-end" ]

                VStretch ->
                    []

        ReverseRow ->
            case valign of
                Top ->
                    [ Html.style "align-self" "flex-start" ]

                VCenter ->
                    [ Html.style "align-self" "center" ]

                Bottom ->
                    [ Html.style "align-self" "flex-end" ]

                VStretch ->
                    []

        Column ->
            case valign of
                Top ->
                    [ Html.style "margin-bottom" "auto" ]

                VCenter ->
                    [ Html.style "margin-top" "auto", Html.style "margin-bottom" "auto" ]

                Bottom ->
                    [ Html.style "margin-top" "auto" ]

                VStretch ->
                    [ Html.style "flex-grow" "1" ]

        ReverseColumn ->
            case valign of
                Top ->
                    [ Html.style "margin-bottom" "auto" ]

                VCenter ->
                    [ Html.style "margin-top" "auto", Html.style "margin-bottom" "auto" ]

                Bottom ->
                    [ Html.style "margin-top" "auto" ]

                VStretch ->
                    [ Html.style "flex-grow" "1" ]



-------------------------------------------------------------------------------
-- Borders


type alias Border =
    { radius : Length
    , width : Length
    , color : Color
    }



-------------------------------------------------------------------------------
-- Elements


type alias Element msg =
    Direction
    -> Padding
    -> HAlignment
    -> VAlignment
    -> List (Html.Attribute msg)
    -> Html msg


type Layer
    = Behind
    | InFront


type Direction
    = Row
    | ReverseRow
    | Column
    | ReverseColumn


type Wrap
    = NoWrap
    | Wrap
    | ReverseWrap



-------------------------------------------------------------------------------
-- User-facing functions


none : Element msg
none =
    stack Row NoWrap [ \_ _ _ _ _ -> Html.div [] [] ]


alignTop : Element msg -> Element msg
alignTop inner dir padding halign valign =
    inner dir padding halign (addVAlignment valign Top)


alignRight : Element msg -> Element msg
alignRight inner dir padding halign =
    inner dir padding (addHAlignment halign Right)


alignBottom : Element msg -> Element msg
alignBottom inner dir padding halign valign =
    inner dir padding halign (addVAlignment valign Bottom)


alignLeft : Element msg -> Element msg
alignLeft inner dir padding halign =
    inner dir padding (addHAlignment halign Left)


centerX : Element msg -> Element msg
centerX inner dir padding halign =
    inner dir padding (addHAlignment halign HCenter)


centerY : Element msg -> Element msg
centerY inner dir padding halign valign =
    inner dir padding halign (addVAlignment valign VCenter)


center : Element msg -> Element msg
center =
    centerX >> centerY


pad : Padding -> Element msg -> Element msg
pad p inner dir padding =
    inner dir (addPadding padding p)


padLeft : Float -> Element msg -> Element msg
padLeft x =
    pad { noPadding | left = Pixels x }


padTop : Float -> Element msg -> Element msg
padTop x =
    pad { noPadding | top = Pixels x }


padRight : Float -> Element msg -> Element msg
padRight x =
    pad { noPadding | right = Pixels x }


padBottom : Float -> Element msg -> Element msg
padBottom x =
    pad { noPadding | bottom = Pixels x }


padLeftPct : Float -> Element msg -> Element msg
padLeftPct x =
    pad { noPadding | left = Percent x }


padTopPct : Float -> Element msg -> Element msg
padTopPct x =
    pad { noPadding | top = Percent x }


padRightPct : Float -> Element msg -> Element msg
padRightPct x =
    pad { noPadding | right = Percent x }


padBottomPct : Float -> Element msg -> Element msg
padBottomPct x =
    pad { noPadding | bottom = Percent x }


stack : Direction -> Wrap -> List (Element msg) -> Element msg
stack dir wrap es outerDir padding halign valign attribs =
    let
        dirStr =
            case dir of
                Row ->
                    ""

                ReverseRow ->
                    "row-reverse"

                Column ->
                    "column"

                ReverseColumn ->
                    "column-reverse"

        wrapStr =
            case wrap of
                NoWrap ->
                    ""

                Wrap ->
                    "wrap"

                ReverseWrap ->
                    "wrap-reverse"
    in
    Html.div
        (Html.style "display" "flex"
            :: Html.style "flex-flow" (dirStr ++ " " ++ wrapStr)
            :: Html.style "align-items" "stretch"
            :: renderPadding padding
            ++ renderHAlignment outerDir halign
            ++ renderVAlignment outerDir valign
            ++ attribs
        )
    <|
        List.map (\e -> e dir noPadding HStretch VStretch []) es


row : List (Element msg) -> Element msg
row =
    stack Row NoWrap


wrappedRow : List (Element msg) -> Element msg
wrappedRow =
    stack Row Wrap


column : List (Element msg) -> Element msg
column =
    stack Column NoWrap


applyPadding : Element msg -> Element msg
applyPadding inner dir padding =
    if padding == noPadding then
        inner dir padding

    else
        stack Row NoWrap [ inner ] dir padding


background : Color -> Element msg -> Element msg
background color =
    attribute (Html.style "background-color" <| renderColor color)
        >> applyPadding


border : Border -> Element msg -> Element msg
border b =
    if isZeroLength b.width then
        identity

    else
        attributes
            [ Html.style "border-radius" <| renderLength b.radius
            , Html.style "border-width" <| renderLength b.width
            , Html.style "border" <| "solid " ++ renderColor b.color
            ]
            >> applyPadding


attribute : Html.Attribute msg -> Element msg -> Element msg
attribute attrib =
    attributes [ attrib ]


attributes : List (Html.Attribute msg) -> Element msg -> Element msg
attributes attribs inner dir padding halign valign outerAttribs =
    inner dir padding halign valign <| outerAttribs ++ attribs


text : String -> Element msg
text s =
    stack Row
        NoWrap
        [ \_ _ _ _ _ -> Html.text s
        ]



-------------------------------------------------------------------------------
-- Rendering


layout : List (Html.Attribute msg) -> Element msg -> Html msg
layout attribs el =
    row [ el ] Row noPadding HStretch VStretch attribs
