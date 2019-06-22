module Frontend exposing (Model, app, subscriptions)

import Browser.Dom as Dom
import Browser.Events as Keyboard
import Debug exposing (toString)
import Dict exposing (..)
import Html exposing (Html, input, text)
import Html.Attributes exposing (autofocus, id, placeholder, style, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode
import Lamdera.Frontend
import Lamdera.Types exposing (..)
import Msg exposing (..)
import Task


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.Frontend.application is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Lamdera.Frontend.application
        { init = \_ _ -> init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "Lamdera board app"
                , body = [ view model ]
                }
        , subscriptions = subscriptions
        , onUrlChange = \_ -> FNoop
        , onUrlRequest = \_ -> FNoop
        }


subscriptions model =
    Sub.batch
        []


type alias Model =
    { gameState : GameState
    }


init : ( Model, Cmd FrontendMsg )
init =
    ( { gameState = Unstarted }, sendToBackend ClientJoined )


view : Model -> Html FrontendMsg
view model =
    Html.div []
        [ gameView model
        , Html.div [ onClick AdminRestartGame ] [ text "Restart the game" ]
        , Html.div [ onClick AdminStartGame ] [ text "Start the game" ]
        ]


gameView model =
    case model.gameState of
        Unstarted ->
            Html.div []
                [ Html.div
                    [ style "width" "400px"
                    , style "height" "200px"
                    , style "position" "relative"
                    , style "background-color" "grey"
                    ]
                    [ Html.text "Game will start shortly!" ]
                ]

        Active mChoice ->
            case mChoice of
                Nothing ->
                    Html.div []
                        [ Html.div
                            [ style "width" "400px"
                            , style "height" "200px"
                            , style "position" "relative"
                            , style "background-color" "red"
                            , onClick (ChoiceMade Red)
                            ]
                            [ Html.text "Red" ]
                        , Html.div
                            [ style "width" "400px"
                            , style "height" "200px"
                            , style "position" "relative"
                            , style "background-color" "blue"
                            , onClick (ChoiceMade Blue)
                            ]
                            [ Html.text "Blue" ]
                        ]

                Just choice ->
                    Html.div []
                        [ Html.div
                            [ style "width" "400px"
                            , style "height" "200px"
                            , style "position" "relative"
                            , if choice == Red then
                                style "background-color" "red"

                              else
                                style "background-color" "grey"
                            ]
                            [ Html.text "Red" ]
                        , Html.div
                            [ style "width" "400px"
                            , style "height" "200px"
                            , style "position" "relative"
                            , if choice == Blue then
                                style "background-color" "blue"

                              else
                                style "background-color" "grey"
                            ]
                            [ Html.text "Blue" ]
                        ]

        DroppedOut roundNumber ->
            Html.div []
                [ Html.div
                    [ style "width" "400px"
                    , style "height" "200px"
                    , style "position" "relative"
                    , style "background-color" "#grey"
                    ]
                    [ Html.text <| "Oops, you're out! You made it to round " ++ String.fromInt roundNumber ]
                ]

        MissedOut ->
            Html.div []
                [ Html.text "Oops, the game has already started! Please wait for the next one."
                ]

        Winner ->
            Html.div []
                [ Html.text "You're the winner! 🎉 Wow!"
                ]


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        ChoiceMade choice ->
            ( { model | gameState = Active (Just choice) }, sendToBackend (ClientChoiceMade choice) )

        AdminRestartGame ->
            ( model, sendToBackend ClientAdminRestartGame )

        AdminStartGame ->
            ( model, sendToBackend ClientAdminStartGame )

        FNoop ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        Verdict gameState ->
            ( { model | gameState = gameState }, Cmd.none )

        RestartGame ->
            ( { model | gameState = Unstarted }, Cmd.none )


sendToBackend : Msg.ToBackend -> Cmd Msg.FrontendMsg
sendToBackend msg =
    sendToBackendWrapper 1000 (\_ -> FNoop) msg


sendToBackendWrapper : Milliseconds -> (Result WsError () -> Msg.FrontendMsg) -> Msg.ToBackend -> Cmd Msg.FrontendMsg
sendToBackendWrapper =
    Lamdera.Frontend.sendToBackend
