module Session exposing (..)

import Page exposing (Page)
import Browser.Navigation as Nav
import NavBar

type alias Session =
  { key : Nav.Key
  , navModel : NavBar.Model
  }

init : Nav.Key -> NavBar.Model -> Session
init key model = Session key model

getKey : Session -> Nav.Key
getKey session = session.key

updateNavModel :  Session -> NavBar.Model -> Session
updateNavModel session navbarmodel = { session | navModel = navbarmodel }

disableMenu : Session -> Session
disableMenu session = { session | navModel = NavBar.init }