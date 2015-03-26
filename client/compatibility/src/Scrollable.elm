module Scrollable where

import Graphics.Element as E
import Html (..)
import Html.Attributes (..)
import Signal
import Layout (..)
import Color as C
import String

scrollable : Dynamic E.Element -> Dynamic E.Element
scrollable element dimensions =
    let
        innerHtml = Signal.map fromElement (element dimensions)
        innerHtmls = join [innerHtml]
        scrollableHtml = Signal.map2 (\e (w, h) -> div [class "scrollable",
            style ["padding" *= "0px",
                "margin" *= "0px",
                "width" *= ((toString w) ++ "px"),
                "height" *= ((toString h) ++ "px"),
                "position" *= "relative",
                "overflow" *= "auto"]] e) innerHtmls dimensions
    in Signal.map2 (\html (w, h) -> toElement w h html) scrollableHtml dimensions

(*=) : String -> String -> String
attrib *= val = attrib ++ ": " ++ val ++ ";"

style : List String -> Attribute
style xs = attribute "style" (String.concat xs)