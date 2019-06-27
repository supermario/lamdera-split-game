module Backend exposing (Model, app)

import Dict exposing (..)
import Lamdera.Backend
import Lamdera.Types exposing (..)
import List.Extra as List
import Msg exposing (..)
import Set exposing (Set, map)
import Task
import Time


app =
    Lamdera.Backend.application
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , updateFromFrontend = updateFromFrontend
        }


initialPlayerState : Player
initialPlayerState =
    { gameState = Unstarted
    }


type alias Model =
    { roundNumber : Int
    , players : Dict ClientId Player
    , clients : Set ClientId
    }


init : ( Model, Cmd BackendMsg )
init =
    ( { roundNumber = 0, players = Dict.empty, clients = Set.empty }, Cmd.none )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


updateFromFrontend : ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend clientId msg model =
    case Debug.log "updateFromFrontend" msg of
        ClientJoined ->
            let
                gameState =
                    if model.roundNumber > 1 then
                        { gameState = MissedOut }

                    else
                        initialPlayerState
            in
            ( { model
                | clients = Set.insert clientId model.clients
                , players = Dict.insert clientId gameState model.players
              }
            , if model.roundNumber > 1 then
                sendGameState clientId model gameState

              else
                Cmd.none
            )

        ClientChoiceMade choice ->
            ( { model
                | players =
                    Dict.update clientId
                        (\mPlayer ->
                            case mPlayer of
                                Just player ->
                                    Just { player | gameState = Active (Just choice) }

                                Nothing ->
                                    Just { gameState = Active (Just choice) }
                        )
                        model.players
              }
            , Cmd.none
            )

        ClientAdminRestartGame ->
            let
                newModel =
                    { model
                        | players = Dict.map (\k v -> initialPlayerState) model.players
                        , roundNumber = 1
                    }
            in
            ( newModel, sendGameStates newModel )

        ClientAdminStartGame ->
            let
                newModel =
                    { model
                        | players = Dict.map (\k v -> { gameState = Active Nothing }) model.players
                        , roundNumber = 1
                    }
            in
            ( newModel, sendGameStates newModel )

        ClientAdminEndRound ->
            let
                reds =
                    Dict.filter (\k players -> players.gameState == Active (Just Red)) model.players

                blues =
                    Dict.filter (\k players -> players.gameState == Active (Just Blue)) model.players

                newModel =
                    { model
                        | players =
                            Dict.map
                                (\k player ->
                                    case player.gameState of
                                        Active (Just choice) ->
                                            if choice == Red && Dict.size reds == 1 then
                                                { gameState = Winner }

                                            else if choice == Blue && Dict.size blues == 1 then
                                                { gameState = Winner }

                                            else if choice == Red && (Dict.size reds > Dict.size blues) && Dict.size blues /= 0 then
                                                { gameState = DroppedOut model.roundNumber }

                                            else if choice == Blue && (Dict.size reds < Dict.size blues) && Dict.size reds /= 0 then
                                                { gameState = DroppedOut model.roundNumber }

                                            else
                                                { gameState = Active Nothing }

                                        Active Nothing ->
                                            { gameState = DroppedOut model.roundNumber }

                                        _ ->
                                            player
                                )
                                model.players
                    }

                newNewModel =
                    { newModel | roundNumber = model.roundNumber + 1 }
            in
            ( newNewModel, sendGameStates newNewModel )


sendGameStates model =
    model.players
        |> Dict.toList
        |> List.map (\( clientId, player ) -> sendToFrontend clientId (PlayerGameStatus { gameState = player.gameState, roundNumber = model.roundNumber }))
        |> Cmd.batch


sendGameState clientId model player =
    sendToFrontend clientId (PlayerGameStatus { gameState = player.gameState, roundNumber = model.roundNumber })


broadcast clients msg =
    clients
        |> Set.toList
        |> List.map (\clientId -> sendToFrontend clientId msg)
        |> Cmd.batch


sendToFrontend : ClientId -> ToFrontend -> Cmd BackendMsg
sendToFrontend clientId msg =
    sendToFrontendWrapper 1000 clientId (\_ -> Noop) msg


sendToFrontendWrapper : Milliseconds -> ClientId -> (Result WsError () -> Msg.BackendMsg) -> toFrontend -> Cmd Msg.BackendMsg
sendToFrontendWrapper =
    Lamdera.Backend.sendToFrontend
