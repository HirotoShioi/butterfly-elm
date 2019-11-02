module Session exposing (Msg(..), Session, getKey, init, update)

import Browser.Navigation as Nav
import Butterfly.Type exposing (Butterfly, Query, Region(..), initQuery)
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
    | AddCategory String
    | ResetCategory
    | UpdateRegion Region
    | ResetRegion
    | UpdateColor String
    | ResetColor
    | UpdateColorFromModal String
    | UpdateCategoryFromModal String
    | UpdateRegionFromModal Region



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

        AddCategory category ->
            let
                query =
                    session.query

                newQuery =
                    { query | category = Just category }
            in
            { session | query = newQuery }

        ResetCategory ->
            let
                query =
                    session.query

                newQuery =
                    { query | category = Nothing }
            in
            { session | query = newQuery }

        UpdateRegion region ->
            let
                query =
                    session.query

                newQuery =
                    { query | region = Just region }
            in
            { session | query = newQuery }

        ResetRegion ->
            let
                query =
                    session.query

                newQuery =
                    { query | region = Nothing }
            in
            { session | query = newQuery }

        UpdateColor hexString ->
            let
                query =
                    session.query

                newQuery =
                    { query | hexColor = Just hexString }
            in
            { session | query = newQuery }

        UpdateColorFromModal hexString ->
            let
                query =
                    session.query

                newQuery =
                    { query | hexColor = Just hexString }
            in
            { session | query = newQuery, modalContent = Nothing }

        UpdateCategoryFromModal category ->
            let
                query =
                    session.query

                newQuery =
                    { query | category = Just category }
            in
            { session | query = newQuery, modalContent = Nothing }

        UpdateRegionFromModal region ->
            let
                query =
                    session.query

                newQuery =
                    { query | region = Just region }
            in
            { session | query = newQuery, modalContent = Nothing }

        ResetColor ->
            let
                query =
                    session.query

                newQuery =
                    { query | hexColor = Nothing }
            in
            { session | query = newQuery }


init : Nav.Key -> NavBar.Model -> Maybe Butterfly -> Session
init key model modalContent =
    Session key model modalContent initQuery


getKey : Session -> Nav.Key
getKey session =
    session.key
