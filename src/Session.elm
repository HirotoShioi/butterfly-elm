module Session exposing (Msg(..), Session, getKey, init, update)

import Browser.Navigation as Nav
import Butterfly.Query as Query exposing (Query)
import Butterfly.Type exposing (Butterfly, Region(..))
import NavBar


type alias Session =
    { key : Nav.Key
    , navModel : NavBar.Model
    , modalContent : Maybe Butterfly
    , query : Query
    }


type Msg
    = UpdateNavbar NavBar.Model
    | UpdateModal (Maybe Butterfly)
    | DisableModal
    | DisableMenu
    | EnableModal Butterfly
    | FromDictionary Query.Msg
    | FromModal Query.Msg



-- Convert to (Session, Cmd Msg)


update : Msg -> Session -> Session
update msg session =
    case msg of
        UpdateNavbar navbarModel ->
            { session | navModel = navbarModel }

        UpdateModal modalContent ->
            { session | modalContent = modalContent }

        DisableMenu ->
            { session | navModel = NavBar.init }

        DisableModal ->
            { session | modalContent = Nothing }

        EnableModal butterfly ->
            { session | modalContent = Just butterfly }

        FromDictionary queryMsg ->
            let
                updatedQuery =
                    Query.update session.query queryMsg
            in
            { session | query = updatedQuery }

        FromModal queryMsg ->
            let
                updatedQuery =
                    Query.update Query.init queryMsg
            in
            { session | query = updatedQuery, modalContent = Nothing }


init : Nav.Key -> NavBar.Model -> Maybe Butterfly -> Session
init key model modalContent =
    Session key model modalContent Query.init


getKey : Session -> Nav.Key
getKey session =
    session.key
