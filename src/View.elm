module View exposing (viewCalc)

import Html exposing (Html)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import View.Attributes exposing (Attribute)
import List exposing(..)



{-
   Possible Tasks
   1. Change the layout dynamically, based on number of rows and columns
   2. Add configurable types like
       1. Width
       2. height
       3. Background Color
       4. roundedness
       5. Border
       6. style


-}


type alias CalcConfig =
    { 
      fill : String
    , shape : String
    , radius : Float
    , border_padding : Int
    , rx : Float
    , ry : Float
    , box_rx : Float
    , box_ry : Float
    , cols : Float
    , fontsize : Float
    , fontfamily : String
    }


defCalcConfig : CalcConfig
defCalcConfig =
    { 
      fill = "green"  --initialisation
    , shape = "rect"
    , radius = 0  --will be zero for a rectangle
    , border_padding = 10
    , rx = 0
    , ry = 0
    , box_rx = 2
    , box_ry = 2
    , cols = 4
    , fontsize = 15
    , fontfamily = "Serif"
    }


height =
    20


width =
    30

rowgap =
    10

colgap = 
    10


viewButtons : ( Float, Float ) -> String -> msg -> CalcConfig -> Svg msg
viewButtons ( x, y ) label msg config =
    let
        --( transX, transY ) =
        --    ( x + width / 2, y + height / 2 )
        clr = config.fill
        shape = config.shape
        radius = config.radius
        rx = config.rx
        ry = config.ry

        ( transX, transY ) =
                if shape == "circle" then
                    ( x + radius, y + radius )
                else
                    ( x + width / 2, y + height / 2 )

        btn = 
                if shape == "rect" then
                            S.rect
                            [ SA.x (String.fromFloat x)
                            , SA.y (String.fromFloat y)
                            , SA.height (String.fromFloat height)
                            , SA.width (String.fromFloat width)
                            , SA.rx (String.fromFloat config.box_rx)
                            , SA.ry (String.fromFloat config.box_ry)
                            , SA.fill clr
                            , SA.style "stroke-width:0.5;stroke:rgb(0,0,0)"
                            , SA.fillOpacity "0.5"
                            ]
                            []
                else if shape == "circle" then
                            S.circle
                            [ SA.cx (String.fromFloat (transX + 0.5))
                            , SA.cy (String.fromFloat transY)
                            , SA.r (String.fromFloat radius)
                            , SA.fill clr
                            , SA.style "stroke-width:0.5;stroke:rgb(0,0,0)"
                            , SA.fillOpacity "0.5"
                            ]
                            []
                else
                            S.ellipse
                            [ SA.cx (String.fromFloat (transX + 0.5))
                            , SA.cy (String.fromFloat transY)
                            , SA.rx (String.fromFloat rx)
                            , SA.ry (String.fromFloat ry)
                            , SA.fill clr
                            , SA.style "stroke-width:0.5;stroke:rgb(0,0,0)"
                            , SA.fillOpacity "0.5"
                            ]
                            []

    in
    S.g
        [ SE.onClick msg
        ]
        [ 
          btn
        , S.text_
            [ SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            , SA.fontSize (String.fromFloat config.fontsize)
            , SA.fontFamily config.fontfamily
            , SA.transform ("translate(" ++ String.fromFloat transX ++ " " ++ String.fromFloat transY ++ ")")
            ]
            [ S.text label ]
        ]


viewDisplay : ( Float, Float ) -> ( Float, Float ) -> String -> Svg msg
viewDisplay ( x, y ) ( w, h ) str =
    let
        ( transX, transY ) =
            ( x + w / 2, y + h / 2 )
    in
    S.g
        []
        [ S.rect
            [ SA.x (String.fromFloat x)
            , SA.y (String.fromFloat y)
            , SA.height (String.fromFloat h)
            , SA.width (String.fromFloat w)
            , SA.rx "1"
            , SA.style "fill:rgb(143 143 237);stroke-width:0.5;stroke:rgb(0,0,0)"
            , SA.fillOpacity "0"
            ]
            []
        , S.text_
            [ SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            , SA.transform ("translate(" ++ String.fromFloat transX ++ " " ++ String.fromFloat transY ++ ")")
            ]
            [ S.text str ]
        ]



{-
   This is the main view function for calculator
   It takes all the data required to show a calculator
   1. The attributes like height, width etc
   2. The history
   3. The final answer
   4. All the buttons to be shown

-}

getHeight : List( String, msg ) -> Int -> Int -> CalcConfig -> String
getHeight buttons cols rows config = 
            case config.shape of
                "rect" ->
                    String.fromInt (56 + height + ((rows - 1)*(height+rowgap)))
                "circle" ->
                    String.fromInt (56 + ((round config.radius) * 2) + ((rows - 1)*(((round config.radius) * 2)+rowgap)))
                "ellipse" ->
                    String.fromInt (56 + ((round config.ry) * 2) + ((rows - 1)*(((round config.ry) * 2)+rowgap)))
                _ ->
                    String.fromInt (56 + height + ((rows - 1)*(height+rowgap)))

getWidth : List( String, msg ) -> Int -> CalcConfig -> String
getWidth buttons cols config = 
            case config.shape of
                "rect" ->
                    String.fromInt ((cols * width) + (cols - 1)*colgap + 2)
                "circle" ->
                    String.fromInt ((cols * 2 * (round config.radius)) + (cols - 1)*colgap + 2)
                "ellipse" ->
                    String.fromInt ((cols * 2 * (round config.rx)) + (cols - 1)*colgap + 2)
                _ ->
                    String.fromInt ((cols * width) + (cols - 1)*colgap + 2)



viewCalc : List (Attribute CalcConfig) -> String -> String -> List ( String, msg ) -> Html msg
viewCalc edits history answer buttons =
    let
        config =
            List.foldl (\f a -> f a) defCalcConfig edits
        rows = 
            ceiling ((toFloat (length buttons))/(config.cols))

        buttonGroups =
            splitAtEvery (round config.cols) buttons

        coordButtons =
            getCoordinatedList 52 buttonGroups config

        maxW =
            case config.shape of
                "rect" ->
                    (List.maximum (List.map (\( ( x, _ ), _ ) -> x) coordButtons)
                        |> Maybe.withDefault 500
                    )
                        + width
                "circle" ->
                    (List.maximum (List.map (\( ( x, _ ), _ ) -> x) coordButtons)
                        |> Maybe.withDefault 500
                    )
                        + 2 * config.radius
                "ellipse" ->
                    (List.maximum (List.map (\( ( x, _ ), _ ) -> x) coordButtons)
                        |> Maybe.withDefault 500
                    )
                        + 2 * config.rx
                _ ->
                    (List.maximum (List.map (\( ( x, _ ), _ ) -> x) coordButtons)
                        |> Maybe.withDefault 500
                    )
                        + width
    in
    S.svg
        [ SA.viewBox ("0 0 " ++ (getWidth buttons (round config.cols) config) ++ " " ++ (getHeight buttons (round config.cols) rows config))
        , SA.height "80vh"
        , SA.style ("border-style:solid;padding:"++(String.fromInt config.border_padding)++"px;")
        ]
        (viewDisplay ( 0, 0 ) ( maxW, 20 ) history
            :: viewDisplay ( 0, 25 ) ( maxW, 20 ) answer
            :: List.map
                (\( c, ( l, m ) ) -> viewButtons c l m config)
                coordButtons
        )



-- I N T E R N A L      H E L P E R S


splitAtEvery : Int -> List a -> List (List a)
splitAtEvery index lst =
    case lst of
        [] ->
            []

        _ ->
            List.append [ List.take index lst ] (splitAtEvery index (List.drop index lst))


getCoordinatedList : Float -> List (List ( String, msg )) -> CalcConfig -> List ( ( Float, Float ), ( String, msg ) )
getCoordinatedList initY lst config =
    let
        horizontal = 
            case config.shape of
                "rect" ->
                    width
                "circle" ->
                    2 * config.radius
                "ellipse" ->
                    2 * config.rx
                _ ->
                    width
        
        vertical = 
            case config.shape of
                "rect" ->
                    height
                "circle" ->
                    2 * config.radius
                "ellipse" ->
                    2 * config.ry
                _ ->
                    height

        assignXCoord y list =
            List.foldl (\e ( x, fe ) -> ( x + horizontal + colgap, List.append fe [ ( ( x, y ), e ) ] )) ( 0, [] ) list
                |> Tuple.second

        ( _, yLists ) =
            List.foldl (\l ( y, ls ) -> ( y + vertical + rowgap, List.append ls [ ( y, l ) ] )) ( initY, [] ) lst
    in
    List.map (\( y, l ) -> assignXCoord y l) yLists
        |> List.concat

