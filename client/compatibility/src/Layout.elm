module Layout where

import Graphics.Element (..)
import Signal
import List
import Color

type alias Static a = (Signal Int, Signal Int -> Signal a)
type alias Dynamic a = Signal (Int, Int) -> Signal a

fit : (Element -> Element -> Element) -> ((Int, Int) -> Int -> (Int, (Int, Int))) -> Static Element -> Dynamic Element -> Dynamic Element
fit combiner f static dynamic dimensions =
    let
        dimensions' : Signal (Int, (Int, Int))
        dimensions' = Signal.map2 f dimensions (fst static)
    in Signal.map2 combiner (snd static (Signal.map fst dimensions')) (dynamic (Signal.map snd dimensions'))

over : Static Element -> Dynamic Element -> Dynamic Element
over = fit above (\(x, y) h -> (x, (x, y - h)))

under : Static Element -> Dynamic Element -> Dynamic Element
under = fit below (\(x, y) h -> (x, (x, y - h)))

leftOf : Static Element -> Dynamic Element -> Dynamic Element
leftOf = fit beside (\(x, y) w -> (y, (x - w, y)))

rightOf : Static Element -> Dynamic Element -> Dynamic Element
rightOf = fit (flip beside) (\(x, y) w -> (y, (x - w, y)))

ofWidth : Dynamic Element -> Signal Int -> Static Element
ofWidth dynamic w = (w, \h -> dynamic (Signal.map2 (,) w h))

ofHeight : Dynamic Element -> Signal Int -> Static Element
ofHeight dynamic h = (h, \w -> dynamic (Signal.map2 (,) w h))

space : Dynamic Element
space = Signal.map (uncurry spacer)

pole : Signal Int -> Static Element
pole w = (w, Signal.map2 spacer w)

bar : Signal Int -> Static Element
bar h = (h, Signal.map2 (flip spacer) h)

(#) : Dynamic Element -> (Element -> Element) -> Dynamic Element
dynamic # f = \dimensions -> Signal.map f (dynamic dimensions)

(##) : Static Element -> (Element -> Element) -> Static Element
static ## f = (fst static, \dimension -> Signal.map f (snd static dimension))

(~#) : Dynamic Element -> (Signal Element -> Dynamic Element) -> Dynamic Element
dynamic ~# f = \dimensions -> f (dynamic dimensions) dimensions

join : List (Signal a) -> Signal (List a)
join elements = case elements of
    [] -> Signal.constant []
    (x::xs) -> Signal.map2 (::) x (join xs)

grid : Direction -> List (Dynamic Element) -> Dynamic Element
grid direction dynamics dimensions =
    let
        newDimensions : Signal (Int, Int)
        newDimensions = Signal.map (\(x, y) -> case direction == up || direction == down of
            True -> (x, y // (List.length dynamics))
            False -> (x // (List.length dynamics), y)) dimensions
    in Signal.map (flow direction) (join (List.map (\dynamic -> dynamic newDimensions) dynamics))

stack : Direction -> List (Static Element) -> Static Element
stack direction statics =
    let
        fixedDimension = Signal.map (List.sum) << join << List.map fst <| statics
        newElement dimension = Signal.map (flow direction) << join << List.map (\static -> static dimension) << List.map snd <| statics
    in (fixedDimension, newElement)

inset : Int -> Dynamic Element -> Dynamic Element
inset amount dynamic dimensions =
    let
        innerDimensions = Signal.map (\(w, h) -> (w - 2 * amount, h - 2 * amount)) dimensions
        innerElement = dynamic innerDimensions
    in lay middle innerElement dimensions

lay : Position -> Signal Element -> Dynamic Element
lay position element dimensions =
    let
        width = Signal.map fst dimensions
        height = Signal.map snd dimensions
    in Signal.map4 container width height (Signal.constant position) element

border : Int -> Color.Color -> Dynamic Element -> Dynamic Element
border x c e = (e |> inset x) # color c