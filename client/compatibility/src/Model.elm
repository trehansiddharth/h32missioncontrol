module Model where

type alias SongInfo = { file : String, title : String, album : String, artist : String, time : Int }

type SongCommand = Play | Pause | Next

type SearchResultState = NoSearch | LoadingResults | SearchResults (List SongInfo)

type alias QueryResult a = { command : String, arguments : List String, waiting : Bool, result : List a }

type alias MusicStatus = { volume : Int, elapsed : Maybe Int, song : Maybe SongInfo, state : String }