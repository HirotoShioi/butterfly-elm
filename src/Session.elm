module Session exposing (..)

import Page exposing (Page)
import Browser.Navigation as Nav

type alias Session =
  { key : Nav.Key
  , pageModel : Page.Model
  }

init : Nav.Key -> Page.Model -> Session
init key model = Session key model

getKey : Session -> Nav.Key
getKey session = session.key

setPage : Page -> Session -> Session
setPage page session = 
  let newPage = session.pageModel |> Page.setPage page
  in { session | pageModel = newPage }