module Kui exposing
    ( Border
    , Color
    , Direction(..)
    , Element(..)
    , HAlignment(..)
    , Length(..)
    , Padding
    , Primitive(..)
    , Rectangle
    , VAlignment(..)
    , Wrap(..)
    , addHAlignment
    , addLength
    , addPadding
    , addVAlignment
    , align
    , alignBottom
    , alignLeft
    , alignRight
    , alignTop
    , attribute
    , attributes
    , background
    , behind
    , border
    , center
    , centerX
    , centerY
    , column
    , empty
    , emptyRectangle
    , inFront
    , isPurePrimitive
    , isZeroLength
    , layout
    , layoutPrimitive
    , noPadding
    , pad
    , padBottom
    , padBottomPct
    , padLeft
    , padLeftPct
    , padRight
    , padRightPct
    , padTop
    , padTopPct
    , primitive
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
                    String.fromFloat p ++ "pct"

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
    { left : Length
    , top : Length
    , right : Length
    , bottom : Length
    }


noPadding : Padding
noPadding =
    { left = zeroLength, top = zeroLength, right = zeroLength, bottom = zeroLength }


addPadding : Padding -> Padding -> Padding
addPadding p1 p2 =
    { left = addLength p1.left p2.left
    , top = addLength p1.top p2.top
    , right = addLength p1.right p2.right
    , bottom = addLength p1.bottom p2.bottom
    }



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



-------------------------------------------------------------------------------
-- Rectangles


type alias Rectangle =
    { fill : Color
    , border : Border
    }


emptyRectangle : Rectangle
emptyRectangle =
    { fill = rgba 0 0 0 0
    , border =
        { radius = zeroLength
        , width = zeroLength
        , color = rgba 0 0 0 0
        }
    }


type alias Border =
    { radius : Length
    , width : Length
    , color : Color
    }



-------------------------------------------------------------------------------
-- Elements


type Element msg
    = Element Padding HAlignment VAlignment (List (Html.Attribute msg)) (Primitive msg)


type Primitive msg
    = Layer Layer (Element msg) (List (Element msg))
    | Stack Direction Wrap (List (Element msg))
    | Html ((List (Html.Attribute msg) -> Element msg -> Html.Html msg) -> List (Html.Attribute msg) -> Html.Html msg)
      -- TODO: Are these subsumed by Html?
    | Rect Rectangle
    | Text String


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


primitive : Primitive msg -> Element msg
primitive =
    Element noPadding HStretch VStretch []


isPurePrimitive : Element msg -> Maybe (Primitive msg)
isPurePrimitive (Element p h v attrs prim) =
    if p == noPadding && h == HStretch && v == VStretch && attrs == [] then
        Just prim

    else
        Nothing



-------------------------------------------------------------------------------
-- User-facing functions


empty : Element msg
empty =
    Element noPadding HStretch VStretch [] <| Stack Row NoWrap []


align : HAlignment -> VAlignment -> Element msg -> Element msg
align h1 v1 (Element p2 h2 v2 as1 e2) =
    Element p2 (addHAlignment h1 h2) (addVAlignment v1 v2) as1 e2


alignLeft : Element msg -> Element msg
alignLeft =
    align Left VStretch


alignTop : Element msg -> Element msg
alignTop =
    align HStretch Top


alignRight : Element msg -> Element msg
alignRight =
    align Right VStretch


alignBottom : Element msg -> Element msg
alignBottom =
    align HStretch Bottom


centerX : Element msg -> Element msg
centerX =
    align HCenter VStretch


centerY : Element msg -> Element msg
centerY =
    align HStretch VCenter


center : Element msg -> Element msg
center =
    align HCenter VCenter


pad : Padding -> Element msg -> Element msg
pad p1 (Element p2 h2 v2 as1 e2) =
    Element (addPadding p1 p2) h2 v2 as1 e2


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


behind : Element msg -> List (Element msg) -> Element msg
behind e es =
    primitive <| Layer Behind e es


inFront : Element msg -> List (Element msg) -> Element msg
inFront e es =
    primitive <| Layer InFront e es


stack : Direction -> Wrap -> List (Element msg) -> Element msg
stack d w es =
    case es of
        [ e ] ->
            e

        _ ->
            primitive <| Stack d w es


row : List (Element msg) -> Element msg
row =
    stack Row NoWrap


wrappedRow : List (Element msg) -> Element msg
wrappedRow =
    stack Row Wrap


column : List (Element msg) -> Element msg
column =
    stack Column NoWrap


attribute : Html.Attribute msg -> Element msg -> Element msg
attribute a (Element p h v as_ e) =
    Element p h v (a :: as_) e


attributes : List (Html.Attribute msg) -> Element msg -> Element msg
attributes as1 (Element p h v as2 e) =
    Element p h v (as1 ++ as2) e



-- TODO: Can these be done with HTML attributes?


background : Color -> Element msg -> Element msg
background color e =
    behind e [ primitive <| Rect { emptyRectangle | fill = color } ]


border : Border -> Element msg -> Element msg
border b e =
    behind e [ primitive <| Rect { emptyRectangle | border = b } ]


text : String -> Element msg
text =
    primitive << Text



-------------------------------------------------------------------------------
-- Rendering


flexDiv : Direction -> Wrap -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
flexDiv dir wrap attrs =
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
            :: attrs
        )


layout : List (Html.Attribute msg) -> Element msg -> Html msg
layout attrs el =
    flexDiv Row
        NoWrap
        attrs
        [ layoutFlexItem Row [] el
        ]


layoutFlexItem : Direction -> List (Html.Attribute msg) -> Element msg -> Html msg
layoutFlexItem dir attrs (Element padding halign valign elAttrs prim) =
    let
        halignAttrs =
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
                            []

                ReverseRow ->
                    case halign of
                        Left ->
                            [ Html.style "margin-right" "auto" ]

                        HCenter ->
                            [ Html.style "margin-left" "auto", Html.style "margin-right" "auto" ]

                        Right ->
                            [ Html.style "margin-left" "auto" ]

                        HStretch ->
                            []

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

        valignAttrs =
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
                            []

                ReverseColumn ->
                    case valign of
                        Top ->
                            [ Html.style "margin-bottom" "auto" ]

                        VCenter ->
                            [ Html.style "margin-top" "auto", Html.style "margin-bottom" "auto" ]

                        Bottom ->
                            [ Html.style "margin-top" "auto" ]

                        VStretch ->
                            []
    in
    layoutPrimitive dir (Html.style "flex" "1 0 auto" :: halignAttrs ++ valignAttrs ++ elAttrs ++ attrs) prim


layoutPrimitive : Direction -> List (Html.Attribute msg) -> Primitive msg -> Html msg
layoutPrimitive dir attrs prim =
    let
        divAttrs e =
            case attrs of
                [] ->
                    e

                _ ->
                    Html.div attrs [ e ]
    in
    case prim of
        Layer layer element es ->
            flexDiv Row NoWrap (Html.style "position" "relative" :: attrs) <|
                let
                    es_ =
                        List.map
                            (\e ->
                                layout
                                    [ Html.style "position" "absolute"
                                    , Html.style "left" "0px"
                                    , Html.style "top" "0px"
                                    , Html.style "width" "100%"
                                    , Html.style "height" "100%"
                                    ]
                                    e
                            )
                            es

                    element_ =
                        layout
                            [ Html.style "z-index" <|
                                case layer of
                                    Behind ->
                                        "1"

                                    InFront ->
                                        "-1"
                            , Html.style "left" "0px"
                            , Html.style "top" "0px"
                            , Html.style "width" "100%"
                            , Html.style "height" "100%"
                            ]
                            element
                in
                element_ :: es_

        Stack dir_ wrap children ->
            flexDiv dir_
                wrap
                attrs
                (List.map (layoutFlexItem dir_ []) children)

        Html f ->
            f layout attrs

        Rect rect ->
            let
                fillCss =
                    [ Html.style "background-color" <| renderColor rect.fill ]

                borderCss =
                    if isZeroLength rect.border.width then
                        []

                    else
                        [ Html.style "border-radius" (renderLength rect.border.radius), Html.style "border-width" (renderLength rect.border.width), Html.style "border-color" (renderColor rect.border.color) ]
            in
            Html.div
                (fillCss
                    ++ borderCss
                    ++ attrs
                )
                []

        Text t ->
            divAttrs <| Html.text t
