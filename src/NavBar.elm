module NavBar exposing (..)

import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseOver, stopPropagationOn)
import Json.Decode as Decode exposing (Decoder)
import Page exposing (Page)
import Route exposing (Route)


type Msg
    = ToggleMenu
    | DisableMenu


type alias Model =
    { isMenuOpen : Bool
    }


init : Model
init =
    Model False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

        DisableMenu ->
            ( { model | isMenuOpen = False }, Cmd.none )


toggleNav : Model -> Model
toggleNav model =
    { model | isMenuOpen = not model.isMenuOpen }


view : Page -> Model -> Html Msg
view page model =
    navbar navbarModifiers
        []
        [ navbarBrand []
            (myNavbarBurger model.isMenuOpen)
            [ navbarItem False
                []
                [ a [ href "/" ]
                    [ img [ src "https://package.elm-lang.org/assets/favicon.ico" ] []
                    ]
                ]
            ]
        , navbarMenu model.isMenuOpen
            []
            [ navbarStart []
                [ navLink page Route.Dictionary Page.dictionaryTitle
                , navLink page Route.Description Page.descriptionTitle
                , navLink page Route.Category Page.categoryTitle
                , navLink page Route.Area Page.areaTitle
                , navLink page Route.Reference Page.referenceTitle
                ]
            , navbarEnd [] []
            ]
        ]


navLink : Page -> Route -> String -> Html msg
navLink page route linkName =
    navbarItemLink (isActive page route) [ Route.href route ] [ text linkName ]


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Page.Home, Route.Home ) ->
            True

        ( Page.Reference, Route.Reference ) ->
            True

        ( Page.Category, Route.Category ) ->
            True

        ( Page.Description, Route.Description ) ->
            True

        ( Page.Area, Route.Area ) ->
            True

        ( Page.Dictionary, Route.Dictionary ) ->
            True

        _ ->
            False


myNavbarBurger : Bool -> Html Msg
myNavbarBurger isMenuOpen =
    navbarBurger isMenuOpen
        [ href "toggle-menu" ]
        [ span [] []
        , span [] []
        , span [] []
        ]
