module Search.Result where
import Model (..)
import Graphics.Element (..)
import Layout (..)
import Signal
import Text
import List
import Color (..)
import Graphics.Input (..)
import Theme (..)

selectedSong : Signal.Channel (Maybe String)
selectedSong = Signal.channel Nothing

searchResults : (Int, Int) -> SearchResultState -> Element
searchResults (width, height) resultState = case resultState of
    NoSearch -> empty
    LoadingResults -> loadingResults (width, height)
    SearchResults songInfos -> flow down << List.map (searchResult width) <| songInfos

searchResult : Int -> SongInfo -> Element
searchResult width songInfo =
    let
        height = 60
        insetAmount = 6
        songIconInsetAmount = 0
        songIconWidth = height - insetAmount * 2 - songIconInsetAmount * 2
        songIconHeight = height - insetAmount * 2 - songIconInsetAmount * 2
        songIconContainerWidth = height - insetAmount * 2
        songIconContainerHeight = height - insetAmount * 2
        queueButtonContainerWidth = 120 - insetAmount * 2
        queueButtonContainerHeight = height - insetAmount * 2
        songIconElement = container songIconContainerWidth songIconContainerWidth middle (songIcon (songIconWidth, songIconHeight))
        queueButtonElement = container queueButtonContainerWidth queueButtonContainerHeight middle queueButton
        innerWidth = width - songIconContainerWidth - queueButtonContainerWidth - insetAmount * 2
        innerHeight = height - insetAmount * 2
        row = flow right [songIconElement,
                            infoCell songInfo.title (innerWidth // 3, innerHeight),
                            infoCell songInfo.album (innerWidth // 3, innerHeight),
                            infoCell songInfo.artist (innerWidth // 3, innerHeight),
                            queueButtonElement] |> color rowBackgroundColor
        queueButton = button (Signal.send selectedSong (Just songInfo.file)) "Queue" |> color buttonColor
        --clickableRow = clickable (Signal.send channel songInfo.file) row
    in container width height middle row

songIcon : (Int, Int) -> Element
songIcon (w, h) = image w h "http://cdn.pitchfork.com/news/53333/402b3c88.jpg" --"https://cdn3.iconfinder.com/data/icons/fez/512/FEZ-01-256.png"

infoCell : String -> (Int, Int) -> Element
infoCell info (w, h) = container w h middle <| Text.plainText info

loadingResults : (Int, Int) -> Element
loadingResults (w, h) = container w h middle <| Text.plainText "Loading results..."