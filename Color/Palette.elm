module Color.Palette exposing (..)

{-| -}


type alias Color =
    { red : Int, blue : Int, green : Int, alpha : Float }


rgba r g b a =
    { red = r, blue = b, green = g, alpha = a }


rgb r g b =
    { red = r, blue = b, green = g, alpha = 1 }


white : Color
white =
    rgb 0xFF 0xFF 0xFF


blue : Color
blue =
    rgb 0x4A 0x90 0xE2


yellow : Color
yellow =
    rgb 0xFD 0xE7 0x4C


green : Color
green =
    rgb 0x9B 0xC5 0x3D


red : Color
red =
    rgb 0xE5 0x59 0x34


orange : Color
orange =
    rgb 0xFA 0x79 0x21
