module Frontend exposing (Model, app, subscriptions)

import Browser exposing (..)
import Browser.Dom
import Browser.Events as Keyboard
import Browser.Navigation as Nav exposing (Key)
import Debug exposing (toString)
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Html exposing (Html, input)
import Html.Attributes exposing (autofocus, id, placeholder, style, type_, value)
import Html.Events exposing (keyCode, on, onInput)
import Json.Decode as Decode
import Lamdera.Frontend
import Lamdera.Types exposing (..)
import Msg exposing (..)
import Task
import Url exposing (Url)
import Widgets exposing (..)


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.Frontend.application is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Lamdera.Frontend.application
        { init = init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "Lamdera board app"
                , body = [ view model ]
                }
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlClicked
        }


subscriptions model =
    Sub.batch
        []


type alias Model =
    { key : Key
    , currentPage : Page
    , gameState : GameState
    , roundNumber : Int
    , playersTotal : Int
    , playersRunning : Int
    }


init : Url -> Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , currentPage = pathToPage url
      , gameState = Unstarted

      -- , gameState = Active Nothing
      -- , gameState = DroppedOut 12
      -- , gameState = Winner
      , roundNumber = 1
      , playersTotal = 0
      , playersRunning = 0
      }
    , sendToBackend ClientJoined
    )


view : Model -> Html FrontendMsg
view model =
    case model.currentPage of
        Home ->
            Html.div []
                [ gameView model
                ]

        Admin ->
            theme model
                [ column [ spacing 20 ]
                    [ row
                        [ Background.color grey, padding 20, onClick AdminRestartGame ]
                        [ text "Restart the game" ]
                    , row
                        [ Background.color grey, padding 20, onClick AdminStartGame ]
                        [ text "Start the game" ]
                    , row
                        [ Background.color grey, padding 20, onClick AdminEndRound ]
                        [ text "End the round" ]
                    ]
                ]


gameView model =
    theme model <|
        [ case model.gameState of
            Unstarted ->
                column [ width fill, spacing 20 ]
                    [ paragraph [ Font.center, Font.size 20 ] [ text "Game will start shortly!" ]
                    , paragraph [ Font.center ] [ text <| String.fromInt model.playersTotal ++ " players joined" ]
                    ]

            Active mChoice ->
                case mChoice of
                    Nothing ->
                        column [ spacing 20, width fill ]
                            [ column [ Font.center, width fill ]
                                [ paragraph [] [ text <| "Round " ++ String.fromInt model.roundNumber ]
                                , paragraph [ Font.center ] [ text <| String.fromInt model.playersRunning ++ "/" ++ String.fromInt model.playersTotal ++ " players left" ]
                                ]
                            , row [ width fill ]
                                [ row
                                    [ Font.center
                                    , height (px 200)
                                    , width fill
                                    , Background.color red
                                    , onClick (ChoiceMade Red)
                                    ]
                                    [ paragraph [] [ text "Red" ] ]
                                , row
                                    [ Font.center
                                    , height (px 200)
                                    , width fill
                                    , Background.color blue
                                    , onClick (ChoiceMade Blue)
                                    ]
                                    [ paragraph [] [ text "Blue" ] ]
                                ]
                            ]

                    Just choice ->
                        column [ spacing 20, width fill ]
                            [ column [ Font.center, width fill ]
                                [ paragraph [] [ text <| "Round " ++ String.fromInt model.roundNumber ]
                                , paragraph [ Font.center ] [ text <| String.fromInt model.playersRunning ++ "/" ++ String.fromInt model.playersTotal ++ " players left" ]
                                ]
                            , row [ width fill ]
                                [ row
                                    [ Font.center
                                    , height (px 200)
                                    , width fill
                                    , Background.color
                                        (if choice == Red then
                                            red

                                         else
                                            grey
                                        )
                                    ]
                                    [ paragraph [] [ text "Red" ] ]
                                , row
                                    [ Font.center
                                    , height (px 200)
                                    , width fill
                                    , Background.color
                                        (if choice == Blue then
                                            blue

                                         else
                                            grey
                                        )
                                    ]
                                    [ paragraph [] [ text "Blue" ] ]
                                ]
                            , paragraph [ Font.center, Font.size 18 ] [ text <| "Waiting for verdict..." ]
                            ]

            DroppedOut roundNumber ->
                column [ spacing 20, width fill ]
                    [ column [ centerX, Font.center, spacing 20 ]
                        [ paragraph [ Font.size 80 ] [ text <| "😭" ]
                        , paragraph [ Font.size 25, Font.bold ] [ text <| "You're out!" ]
                        , paragraph [] [ text <| "You made it to round " ++ String.fromInt roundNumber ]
                        , column [ padding 40, Font.center, width fill, spacing 10 ]
                            [ paragraph [ Font.size 25, Font.bold ] [ text <| String.fromInt model.playersRunning ++ "/" ++ String.fromInt model.playersTotal ++ " players left" ]
                            , paragraph [ Font.center ] [ text <| "Currently round " ++ String.fromInt model.roundNumber ]
                            ]
                        ]
                    ]

            MissedOut ->
                column [ spacing 20, width fill ]
                    [ column [ centerX, Font.center, spacing 20 ]
                        [ paragraph [ Font.size 80 ] [ text <| "😢" ]
                        , paragraph [ Font.size 25, Font.bold ] [ text <| "Oops!" ]
                        , paragraph [] [ text <| "The game has already started!" ]
                        , paragraph [] [ text <| "Please wait for the next one." ]
                        ]
                    ]

            Winner ->
                column [ spacing 20, width fill ]
                    [ column [ centerX, Font.center, spacing 20 ]
                        [ paragraph [ Font.size 80 ] [ text <| "🎉" ]
                        , paragraph [ Font.size 25, Font.bold ] [ text <| "Winner!" ]
                        , paragraph [] [ text <| "Wow! You did it!" ]
                        , paragraph [] [ text <| "You are the most unique person at Elm Europe!" ]
                        ]
                    ]
        ]


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        OpenedPage page ->
            ( { model | currentPage = page }
            , Nav.pushUrl model.key (pageToPath page)
            )

        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( { model | currentPage = pathToPage url }
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            ( { model | currentPage = pathToPage url }, scrollPageToTop )

        ChoiceMade choice ->
            ( { model | gameState = Active (Just choice) }, sendToBackend (ClientChoiceMade choice) )

        AdminRestartGame ->
            ( model, sendToBackend ClientAdminRestartGame )

        AdminStartGame ->
            ( model, sendToBackend ClientAdminStartGame )

        AdminEndRound ->
            ( model, sendToBackend ClientAdminEndRound )

        FNoop ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        PlayerGameStatus status ->
            ( { model
                | gameState = status.gameState
                , roundNumber = status.roundNumber
                , playersTotal = status.playersTotal
                , playersRunning = status.playersRunning
              }
            , Cmd.none
            )

        RestartGame ->
            ( { model | gameState = Unstarted }, Cmd.none )


sendToBackend : Msg.ToBackend -> Cmd Msg.FrontendMsg
sendToBackend msg =
    Lamdera.Frontend.sendToBackend 1000 (\_ -> FNoop) msg


scrollPageToTop =
    Task.perform (\_ -> FNoop) (Browser.Dom.setViewport 0 0)
