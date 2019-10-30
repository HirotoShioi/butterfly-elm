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


init : Nav.Key -> NavBar.Model -> Modal.Model -> Session
init key model modalModel =
    Session key model modalModel


getKey : Session -> Nav.Key
getKey session =
    session.key

-- Needs refactoring

updateNavModel : Session -> NavBar.Model -> Session
updateNavModel session navbarmodel =
    { session | navModel = navbarmodel }


updateModalModel : Session -> Modal.Model -> Session
updateModalModel session modalModel =
    { session | modalModel = modalModel }


enableModal : Session -> Butterfly -> Session
enableModal session butterfly =
    let
        modalModel =
            session.modalModel

        newModalModel =
            { modalModel | mButterfly = Just butterfly }
    in
    { session | modalModel = newModalModel }


disableModal : Session -> Session
disableModal session =
    let
        modalModel =
            session.modalModel

        newModalModel =
            { modalModel | mButterfly = Nothing }
    in
    { session | modalModel = newModalModel }


disableMenu : Session -> Session
disableMenu session =
    { session | navModel = NavBar.init }
