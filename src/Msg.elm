module Msg exposing (BackendMsg(..), Choice(..), FrontendMsg(..), GameState(..), Page(..), Player, RoundNumber, ToBackend(..), ToFrontend(..), choiceFromString, choiceToString, pageToPath, pathToPage)

import Browser exposing (..)
import Browser.Navigation as Nav exposing (Key)
import Dict exposing (..)
import Json.Decode
import Json.Encode
import Lamdera.Types exposing (ClientId, WsError)
import Url exposing (Url)


type FrontendMsg
    = OpenedPage Page
    | UrlClicked UrlRequest
    | UrlChanged Url
    | ChoiceMade Choice
    | AdminRestartGame
    | AdminStartGame
    | AdminEndRound
    | FNoop


type ToBackend
    = ClientJoined
    | ClientChoiceMade Choice
    | ClientAdminRestartGame
    | ClientAdminStartGame
    | ClientAdminEndRound


type BackendMsg
    = Noop


type ToFrontend
    = PlayerGameStatus { gameState : GameState, roundNumber : Int, playersTotal : Int, playersRunning : Int }
    | RestartGame


type Choice
    = Red
    | Blue


choiceToString : Choice -> String
choiceToString choice =
    case choice of
        Red ->
            "Red"

        Blue ->
            "Blue"


choiceFromString : String -> Choice
choiceFromString str =
    case str of
        "Red" ->
            Red

        "Blue" ->
            Blue

        _ ->
            Red


type GameState
    = Unstarted
    | Active (Maybe Choice)
    | DroppedOut RoundNumber
    | MissedOut
    | Winner


type alias RoundNumber =
    Int


type alias Player =
    { gameState : GameState
    }


type Page
    = Home
    | Admin


pageToPath : Page -> String
pageToPath page =
    case page of
        Home ->
            "/"

        Admin ->
            "/admin"


pathToPage : Url -> Page
pathToPage url =
    [ ( "/", Home )
    , ( "/admin", Admin )
    ]
        |> Dict.fromList
        |> Dict.get url.path
        |> Maybe.withDefault Home
