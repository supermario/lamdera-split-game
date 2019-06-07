module LocalDev exposing (main)

import Backend
import Browser
import Browser.Navigation as Navigation
import Frontend
import Html
import Lamdera.Types exposing (Milliseconds, MsgId, WsError)
import Msg exposing (BackendMsg, FrontendMsg, ToBackend, ToFrontend)



-- NOTE: constructor names are important here, and we can't run in optimized mode.
-- Lamdera/core generates msgs with these names but of a different type, and we're unsafeCoercing them into this type.
-- Basically, don't touch this black magic.


type Msg
    = FEMsg Msg.FrontendMsg
    | BEMsg Msg.BackendMsg
    | BEtoFE Milliseconds MsgId Msg.ToFrontend (Result WsError () -> Msg)
    | FEtoBE Milliseconds MsgId Msg.ToBackend (Result WsError () -> Msg)


init : flags -> url -> Navigation.Key -> ( ( Frontend.Model, Backend.Model ), Cmd Msg )
init flags url nav =
    let
        ( feim, fecmd ) =
            Frontend.app.init url nav

        ( beim, becmd ) =
            Backend.app.init
    in
    ( ( feim, beim ), Cmd.batch [ Cmd.map FEMsg fecmd, Cmd.map BEMsg becmd ] )


update : Msg -> ( Frontend.Model, Backend.Model ) -> ( ( Frontend.Model, Backend.Model ), Cmd Msg )
update msg ( fem, bem ) =
    case msg of
        FEMsg frontendMsg ->
            let
                ( nfem, fcmd ) =
                    Frontend.app.update frontendMsg fem
            in
            ( ( nfem, bem ), Cmd.map FEMsg fcmd )

        BEMsg backendMsg ->
            let
                ( nbem, bcmd ) =
                    Backend.app.update backendMsg bem
            in
            ( ( fem, nbem ), Cmd.map BEMsg bcmd )

        BEtoFE ms msgId toFrontend tagger ->
            let
                ( nfem, fcmd ) =
                    Frontend.app.updateFromBackend toFrontend fem

                ( newModel, ncmd ) =
                    -- TODO: delay response by `ms` milliseconds
                    update (tagger (Ok ())) ( nfem, bem )

                _ =
                    Debug.log "BEtoFE" ( toFrontend, tagger (Ok ()) )
            in
            ( newModel, Cmd.batch [ Cmd.map FEMsg fcmd, ncmd ] )

        FEtoBE ms msgId toBackend tagger ->
            let
                ( nbem, bcmd ) =
                    Backend.app.updateFromFrontend "ClientID-local-dev" toBackend bem

                ( newModel, ncmd ) =
                    -- TODO: delay response by `ms` milliseconds
                    update (tagger (Ok ())) ( fem, nbem )

                _ =
                    Debug.log "FEtoBE" ( toBackend, tagger (Ok ()) )
            in
            ( newModel, Cmd.batch [ Cmd.map BEMsg bcmd, ncmd ] )


subscriptions m =
    Sub.none


mapDocument fn { title, body } =
    { title = title, body = List.map (Html.map fn) body }


main : Program () ( Frontend.Model, Backend.Model ) Msg
main =
    Browser.application
        { init = init
        , view = \( fem, _ ) -> mapDocument FEMsg (Frontend.app.view fem)
        , update = update
        , subscriptions = \( fem, _ ) -> Sub.map FEMsg (Frontend.app.subscriptions fem)
        , onUrlRequest = \url -> FEMsg (Frontend.app.onUrlRequest url)
        , onUrlChange = \ureq -> FEMsg (Frontend.app.onUrlChange ureq)
        }
