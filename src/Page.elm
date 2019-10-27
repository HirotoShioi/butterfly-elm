module Page exposing (..)

import Html exposing (Html, main_, text, span, img, a)
import Html.Attributes exposing (..)
import Browser exposing (Document)
import Bulma.CDN exposing (stylesheet)
import Bulma.Modifiers exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Components exposing (..)

import Route exposing (Route(..))

type Page
  = Home
  -- 世界の蝶
  | Reference
  -- 参考文献
  | Category
  -- 蝶の分類
  | Description
  -- 蝶の解説
  | Area
  -- 生物区・蝶分類の地理
  | Dictionary
  -- 570種の図鑑と解説
  | NotFound

type alias Model =
  { page : Page
  , isMenuDropdownOpen : Bool
  , isMenuOpen : Bool 
  }

setPage : Page -> Model -> Model
setPage newPage model = { model | page = newPage }

init : Page -> Model
init page = Model page False False

view : Model -> String -> Html msg -> Document msg
view model title content = 
  { title = title
  , body =
    [ main_ []
      [ stylesheet
      , myNavbar model.page model.isMenuOpen model.isMenuDropdownOpen
      , heroView content
      ]
    ]
  }

heroView : Html msg -> Html msg
heroView content = 
  hero myHeroModifiers []
    [ hero myHeroModifiers []
      [ heroBody []
          [ container [] [ content ] ]
      ]
    ]

myHeroModifiers : HeroModifiers
myHeroModifiers
  = { bold  = False
    , size  = Large
    , color = Default
    }

myNavbarBurger : Bool -> Html msg
myNavbarBurger isMenuOpen
  = navbarBurger isMenuOpen []
    [ span [] []
    , span [] []
    , span [] []
    ]

myNavbarLink : Html msg
myNavbarLink 
  = navbarLink [] 
    [ text "More Junk" 
    ]

myNavbar : Page -> Bool -> Bool -> Html msg
myNavbar page isMenuOpen isMenuDropdownOpen
  = navbar navbarModifiers []
    [ navbarBrand [] (myNavbarBurger isMenuOpen)
      [ navbarItem False []
        [ img [ src "https://package.elm-lang.org/assets/favicon.ico" ] []
        ]
      ]
    , navbarMenu isMenuOpen []
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