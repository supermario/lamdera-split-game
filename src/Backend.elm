module Backend exposing (Model, app)

import Dict exposing (..)
import Lamdera.Backend
import Lamdera.Types exposing (..)
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


newPlayerState : Player
newPlayerState =
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
    let
        x =
            Debug.log "backendModel" model
    in
    case Debug.log "updateFromFrontend" msg of
        ClientJoined ->
            ( { model
                | clients = Set.insert clientId model.clients
                , players = Dict.insert clientId newPlayerState model.players
              }
            , Cmd.none
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
            ( { model | players = Dict.map (\k v -> newPlayerState) model.players }, sendGameStates model.players )

        ClientAdminStartGame ->
            ( { model | players = Dict.map (\k v -> { gameState = Active Nothing }) model.players }, sendGameStates model.players )


sendGameStates players =
    players
        |> Dict.toList
        |> List.map (\( clientId, player ) -> sendToFrontend clientId (Verdict player.gameState))
        |> Cmd.batch


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
