module Msg exposing (BackendMsg(..), Choice(..), FrontendMsg(..), GameState(..), Player, RoundNumber, ToBackend(..), ToFrontend(..))


type FrontendMsg
    = ChoiceMade Choice
    | AdminRestartGame
    | AdminStartGame
    | FNoop


type ToBackend
    = ClientJoined
    | ClientChoiceMade Choice
    | ClientAdminRestartGame
    | ClientAdminStartGame


type BackendMsg
    = Noop


type ToFrontend
    = Verdict GameState
    | RestartGame


type Choice
    = Red
    | Blue


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
