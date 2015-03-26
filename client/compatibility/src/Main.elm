module Main where

import Layout (..)
import Scrollable (..)
import Graphics.Element (..)
import Color (..)
import Window
import Signal (..)
import Graphics.Input (..)
import Graphics.Input.Field (..)
import Text
import List
import String
import Model (..)
import Search.Result (..)
import Search.Box (..)
import Theme (..)
import Util (..)

port searchQueryResult : Signal (Maybe (QueryResult SongInfo))

port musicStatus : Signal MusicStatus

port searchQueryOut : Signal (Maybe String)
port searchQueryOut = (\content -> case content.string of
    "" -> Nothing
    query -> Just query) <~ (sampleOn (subscribe sendQuery) (subscribe searchQuery))

port songCommandOut : Signal (Maybe String)
port songCommandOut = (\state -> case state of
    Nothing -> Nothing
    Just Play -> Just "play"
    Just Pause -> Just "pause"
    Just Next -> Just "next") <~ (subscribe songCommand)

port nextSongOut : Signal (Maybe String)
port nextSongOut = subscribe selectedSong

main : Signal Element
main = Window.dimensions |> layout

layout : Dynamic Element
layout = browsePanel `leftOf` (player `under` view)

browsePanel : Static Element
browsePanel =
    let
        searchElement = searchBox <| constant (230, 100)
        stateElement = stateBox <| constant (230, 80)
        dynamicSearchElement = lay midTop searchElement # color panelBackgroundColor
        dynamicStateElement = lay midBottom stateElement # color panelBackgroundColor
        dynamicElement dimensions =
            let
                dimensionsTop = (\(w, h) -> (w, 100)) <~ dimensions
                dimensionsBottom = (\(w, h) -> (w, h - 100)) <~ dimensions
            in (flow down <~ join [dynamicSearchElement dimensionsTop, dynamicStateElement dimensionsBottom])
        borderedDynamicElement = dynamicElement |> border 1 panelBorderColor
        staticElement = dynamicElement `ofWidth` (constant 230)
    in staticElement

player : Static Element
player = ((\dimensions ->
    let
        width = fst <~ dimensions
        height = snd <~ dimensions
        timeBarWidth = (\w -> w - 20) <~ width
        timeBarHeight = (always 7) <~ height
        timeBarDimensions = (,) <~ timeBarWidth ~ timeBarHeight
    in lay middle (timeBar timeBarDimensions) dimensions) # color playerBackgroundColor) `ofHeight` (constant 80)

stateBox : Dynamic Element
stateBox =
    let
        appropriateButton = (\status -> case status.state of
            "play" -> pauseButton
            "pause" -> playButton
            "stop" -> playButton) <~ musicStatus
        currentSong = .song <~ musicStatus
        buttons = (flow right <~ join [appropriateButton, constant nextButton])
        songInfo info = case info of
            Nothing -> empty
            Just i -> (Text.fromString <| i.title ++ " — " ++ i.artist) |> Text.color white |> Text.centered
        dynamicStatus = lay middle (songInfo << .song <~ musicStatus) # color playerBackgroundColor
        dynamicButtons = lay middle buttons # color playerBackgroundColor
    in grid down [dynamicStatus, dynamicButtons]

timeBar : Dynamic Element
timeBar dimensions =
    let
        width : Signal Int
        width = fst <~ dimensions
        height : Signal Int
        height = snd <~ dimensions
        elapsedTime = (\status -> case status.elapsed of
            Nothing -> 0
            Just x -> x) <~ musicStatus
        totalTime = (\status -> case status.song of
            Nothing -> 1
            Just songInfo -> songInfo.time) <~ musicStatus
        remainingTime = (\total elapsed -> total - elapsed) <~ totalTime ~ elapsedTime
        elapsedWidth : Signal Int
        elapsedWidth = (\elapsed total w -> elapsed * w // total) <~ elapsedTime ~ totalTime ~ width
        remainingWidth : Signal Int
        remainingWidth = (\remaining total w -> remaining * w // total) <~ remainingTime ~ totalTime ~ width
        elapsedBar = color elapsedColor <~ (spacer <~ elapsedWidth ~ height)
        remainingBar = color remainingColor <~ (spacer <~ remainingWidth ~ height)
    in flow right <~ join [elapsedBar, remainingBar]

view : Dynamic Element
view = scrollable (\dimensions ->
    let
        scrollbarWidth = 15
        bufferWidth = 1
        width = (\(w, h) -> w - scrollbarWidth - bufferWidth) <~ dimensions
        dimensions' = (,) <~ width ~ (snd <~ dimensions)
        resultState = (\queries -> case queries of
            Nothing -> NoSearch
            Just query -> case query.waiting || query.command /= "search" of
                True -> LoadingResults
                False -> SearchResults query.result) <~ searchQueryResult
    in searchResults <~ dimensions' ~ resultState) # color viewBackgroundColor |> border 1 viewBorderColor

songCommand : Channel (Maybe SongCommand)
songCommand = channel Nothing

playButton : Element
playButton = button (send songCommand (Just Play)) "Play" |> color buttonColor

pauseButton : Element
pauseButton = button (send songCommand (Just Pause)) "Pause" |> color buttonColor

nextButton : Element
nextButton = button (send songCommand (Just Next)) "Next" |> color buttonColor