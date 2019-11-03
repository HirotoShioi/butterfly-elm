module Session exposing (Msg(..), Session, getKey, init, update)

import Browser.Navigation as Nav
import Butterfly.Api as Api exposing (getButterflies)
import Butterfly.Query as Query exposing (Query)
import Butterfly.Type exposing (Butterfly, Region(..))
import NavBar
import Route


type alias Session =
    { key : Nav.Key
    , navModel : NavBar.Model
    , modalContent : Maybe Butterfly
    , query : Query
    , butterflies : List Butterfly
    }


type Msg
    = UpdateNavbar NavBar.Model
    | UpdateModal (Maybe Butterfly)
    | DisableModal
    | DisableMenu
    | EnableModal Butterfly
    | FromDictionary Query.Msg
    | FromModal Query.Msg
    | GotButterflyResponse Api.Msg



-- Convert to (Session, Cmd Msg)


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        UpdateNavbar navbarModel ->
            ( { session | navModel = navbarModel }, Cmd.none )

        UpdateModal modalContent ->
            ( { session | modalContent = modalContent }, Cmd.none )

        DisableMenu ->
            ( { session | navModel = NavBar.init }, Cmd.none )

        DisableModal ->
            ( { session | modalContent = Nothing }, Cmd.none )

        EnableModal butterfly ->
            ( { session | modalContent = Just butterfly }, Cmd.none )

        FromDictionary queryMsg ->
            let
                updatedQuery =
                    Query.update session.query queryMsg
            in
            ( { session | query = updatedQuery }, Cmd.none )

        FromModal queryMsg ->
            let
                updatedQuery =
                    Query.update Query.init queryMsg
            in
            ( { session | query = updatedQuery, modalContent = Nothing }, Cmd.none )

        GotButterflyResponse apiMsg ->
            case apiMsg of
                Api.GotButterflies (Ok butterflies) ->
                    ( { session | butterflies = butterflies }, Cmd.none )

                Api.GotButterflies (Err _) ->
                    let
                        key =
                            getKey session

                        route =
                            Route.routeToString Route.Error
                    in
                    ( session, Nav.pushUrl key route )


init : Nav.Key -> NavBar.Model -> Maybe Butterfly -> ( Session, Cmd Msg )
init key model modalContent =
    ( Session key model modalContent Query.init [], Cmd.map GotButterflyResponse getButterflies )


getKey : Session -> Nav.Key
getKey session =
    session.key
