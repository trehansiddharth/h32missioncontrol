module Search.Box where
import Model (..)
import Graphics.Element (..)
import Graphics.Input.Field (..)
import Layout (..)
import Signal
import Text
import List
import Color (..)
import Graphics.Input (..)
import Theme (..)

searchQuery : Signal.Channel Content
searchQuery = Signal.channel noContent

sendQuery : Signal.Channel Bool
sendQuery = Signal.channel False

searchBoxField : Dynamic Element
searchBoxField dimensions =
    let
        fieldElement = Signal.map (field defaultStyle (Signal.send searchQuery) "Search for music...") (Signal.subscribe searchQuery)
        w = Signal.map fst dimensions
        h = Signal.map snd dimensions
    in fieldElement |> Signal.map2 width w |> Signal.map2 height h

searchBoxButton : Dynamic Element
searchBoxButton dimensions =
    let
        buttonElement = button (Signal.send sendQuery True) "Search" |> color buttonColor
        w = Signal.map fst dimensions
        h = Signal.map snd dimensions
    in Signal.constant buttonElement |> Signal.map2 width w |> Signal.map2 height h

searchBox : Dynamic Element
searchBox = grid down [searchBoxField, searchBoxButton] |> inset 10