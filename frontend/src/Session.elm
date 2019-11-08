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
    , query : Query
    , butterflies : Result String (List Butterfly)
    }


type Msg
    = DisableMenu
    | FromDictionary Query.Msg
    | FromDetail Query.Msg
    | GotButterflyResponse Api.Msg
    | GotNavMessage NavBar.Msg



-- Convert to (Session, Cmd Msg)


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        DisableMenu ->
            ( { session | navModel = NavBar.init }, Cmd.none )

        FromDictionary queryMsg ->
            let
                updatedQuery =
                    Query.update session.query queryMsg
            in
            ( { session | query = updatedQuery }, Cmd.none )

        FromDetail queryMsg ->
            let
                updatedQuery =
                    Query.update Query.init queryMsg
            in
            ( { session | query = updatedQuery }
            , Nav.pushUrl (getKey session) (Route.routeToString Route.Dictionary)
            )

        GotButterflyResponse apiMsg ->
            case apiMsg of
                Api.GotButterflies (Ok butterflies) ->
                    ( { session | butterflies = Ok butterflies }, Cmd.none )

                Api.GotButterflies (Err _) ->
                    ( { session | butterflies = Err "Failed to load butterfly data" }, Cmd.none )

        GotNavMessage navMsg ->
            let
                ( updatedNavModel, navCmd ) =
                    NavBar.update navMsg session.navModel
            in
            ( { session | navModel = updatedNavModel }
            , Cmd.map GotNavMessage navCmd
            )


init : Nav.Key -> NavBar.Model -> ( Session, Cmd Msg )
init key model =
    ( Session key model Query.init (Ok []), Cmd.map GotButterflyResponse getButterflies )


getKey : Session -> Nav.Key
getKey session =
    session.key
