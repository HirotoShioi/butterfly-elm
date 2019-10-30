module Session exposing (..)

import Browser.Navigation as Nav
import Butterfly.Type exposing (Butterfly)
import Modal
import NavBar
import Page exposing (Page)


type alias Session =
    { key : Nav.Key
    , navModel : NavBar.Model
    , modalModel : Modal.Model
    }

type Msg
    = UpdateNavbar NavBar.Model
    | UpdateModal Modal.Model
    | DisableModal
    | DisableMenu
    | EnableModal Butterfly

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


init : Nav.Key -> NavBar.Model -> Modal.Model -> Session
init key model modalModel =
    Session key model modalModel


getKey : Session -> Nav.Key
getKey session =
    session.key
