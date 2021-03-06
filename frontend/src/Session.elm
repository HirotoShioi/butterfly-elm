module Session exposing (Msg(..), Session, getNav, init, update)

import Butterfly.Api as Api exposing (getButterflies)
import Butterfly.Type exposing (Butterfly, Region(..))
import NavBar
import Navigation exposing (Nav)


type alias Session =
    { nav : Nav Msg
    , navModel : NavBar.Model
    , butterflies : Result String (List Butterfly)
    }


type Msg
    = DisableMenu
    | GotButterflyResponse Api.Msg
    | GotNavMessage NavBar.Msg



-- Convert to (Session, Cmd Msg)


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        DisableMenu ->
            ( { session | navModel = NavBar.init }, Cmd.none )

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


init : Nav Msg -> NavBar.Model -> ( Session, Cmd Msg )
init nav model =
    ( Session nav model (Ok [])
    , Cmd.map GotButterflyResponse getButterflies
    )


getNav : Session -> Nav Msg
getNav session =
    session.nav
