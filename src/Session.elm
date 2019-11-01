module Session exposing (..)

import Browser.Navigation as Nav
import Butterfly.Type exposing (Butterfly, Query, Region(..), initQuery)
import Modal
import NavBar
import Page exposing (Page)


type alias Session =
    { key : Nav.Key
    , navModel : NavBar.Model
    , modalModel : Modal.Model
    , query : Query
    }


type Msg
    = UpdateNavbar NavBar.Model
    | UpdateModal Modal.Model
    | DisableModal
    | DisableMenu
    | EnableModal Butterfly
    | AddCategory String
    | ResetCategory
    | UpdateRegion Region
    | ResetRegion
    | UpdateColor String
    | ResetColor



-- Convert to (Session, Cmd Msg)


update : Msg -> Session -> Session
update msg session =
    case msg of
        UpdateNavbar navbarModel ->
            { session | navModel = navbarModel }

        UpdateModal modalModel ->
            { session | modalModel = modalModel }

        DisableMenu ->
            { session | navModel = NavBar.init }

        DisableModal ->
            let
                modalModel =
                    session.modalModel

                newModalModel =
                    { modalModel | mButterfly = Nothing }
            in
            { session | modalModel = newModalModel }

        EnableModal butterfly ->
            let
                modalModel =
                    session.modalModel

                newModalModel =
                    { modalModel | mButterfly = Just butterfly }
            in
            { session | modalModel = newModalModel }

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

        ResetColor ->
            let
                query =
                    session.query

                newQuery =
                    { query | hexColor = Nothing }
            in
            { session | query = newQuery }


init : Nav.Key -> NavBar.Model -> Modal.Model -> Session
init key model modalModel =
    Session key model modalModel initQuery


getKey : Session -> Nav.Key
getKey session =
    session.key
