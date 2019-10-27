module NavBar exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (stopPropagationOn, onMouseOver)
import Bulma.Modifiers exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Components exposing (..)
import Json.Decode as Decode exposing (Decoder)

import Route exposing (Route)
import Page exposing (Page(..))

type Msg
  = ToggleMenu
  | DisableMenu

type alias Model =
  { isMenuOpen : Bool
  }

init : Model
init = Model False

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleMenu -> ({ model | isMenuOpen = not model.isMenuOpen }, Cmd.none)
    DisableMenu -> ({ model | isMenuOpen = False}, Cmd.none )

toggleNav : Model -> Model
toggleNav model = { model | isMenuOpen = not model.isMenuOpen }

view : Page -> Model -> Html Msg
view page model
  = navbar navbarModifiers []
    [ navbarBrand [] (myNavbarBurger True)
      [ navbarItem False []
        [ img [ src "https://package.elm-lang.org/assets/favicon.ico" ] []
        ]
      ]
    , navbarMenu model.isMenuOpen []
      [ navbarStart [] 
        [ navLink page Route.Reference "参考文献"
        , navLink page Route.Category "蝶の分類"
        , navLink page Route.Description "蝶の解説"
        , navLink page Route.Area "生物区・蝶分類の地理"
        , navLink page Route.Dictionary "蝶の図鑑"
        ]
      , navbarEnd [] []
      ]
    ]

navLink : Page -> Route -> String -> Html msg
navLink page route linkName =
  navbarItemLink (isActive page route) [Route.href route] [text linkName]

isActive : Page -> Route -> Bool
isActive page route =
  case ( page, route ) of
    ( Reference, Route.Reference ) ->
      True
    ( Category, Route.Category ) ->
      True
    ( Description, Route.Description ) ->
      True
    ( Area, Route.Area ) ->
      True
    ( Dictionary, Route.Dictionary ) ->
      True
    _ ->
        False

myNavbarBurger : Bool -> Html Msg
myNavbarBurger isMenuOpen
  = navbarBurger isMenuOpen [href "toggle-menu"]
    [ span [] []
    , span [] []
    , span [] []
    ]
