module View.Attributes exposing (..)


type alias Attribute c =
    c -> c



{- Function to set the width attribute of CalcConfig -}



color: String -> Attribute { c | fill : String }
color str =
    \cc ->
        { cc | fill = str }

shape: String -> Attribute { c | shape : String }
shape str =
    \cc ->
        { cc | shape = str }
        
radius: Float -> Attribute { c | radius : Float }
radius rad =
    \cc ->
        { cc | radius = rad }

cols: Float -> Attribute { c | cols : Float }
cols columns =
    \cc ->
        { cc | cols = columns }

rx: Float -> Attribute { c | rx : Float }
rx rad =
    \cc ->
        { cc | rx = rad }

ry: Float -> Attribute { c | ry : Float }
ry rad =
    \cc ->
        { cc | ry = rad }

box_rx: Float -> Attribute { c | box_rx : Float }
box_rx rad =
    \cc ->
        { cc | box_rx = rad }

box_ry: Float -> Attribute { c | box_ry : Float }
box_ry rad =
    \cc ->
        { cc | box_ry = rad }

fontsize: Float -> Attribute { c | fontsize : Float }
fontsize fsize =
    \cc ->
        { cc | fontsize = fsize }

fontfamily: String -> Attribute { c | fontfamily : String }
fontfamily fam =
    \cc ->
        { cc | fontfamily = fam }

padding: Int -> Attribute { c | border_padding : Int }
padding pad =
    \cc ->
        { cc | border_padding = pad }