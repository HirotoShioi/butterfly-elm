module NavBar exposing (Model, Msg(..), init, update, view)

import Bulma.Components as B
import Butterfly.Query as Query
import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)
import Page exposing (Page)
import Route exposing (Route)


type Msg
    = ToggleMenu
    | DisableMenu


type alias Model =
    Bool


init : Model
init =
    False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleMenu ->
            ( not model, Cmd.none )

        DisableMenu ->
            ( False, Cmd.none )


view : Page -> Model -> Html Msg
view page isOpen =
    B.navbar B.navbarModifiers
        []
        [ B.navbarBrand []
            (myNavbarBurger isOpen)
            [ B.navbarItem False
                []
                [ a [ Route.href Route.Home ]
                    [ img [ src "https://package.elm-lang.org/assets/favicon.ico" ] []
                    ]
                ]
            ]
        , B.navbarMenu isOpen
            []
            [ B.navbarStart []
                [ navLink page (Route.Dictionary Query.init) <| Page.toTitle Page.Dictionary
                , navLink page Route.Description <| Page.toTitle Page.Description
                , navLink page Route.Category <| Page.toTitle Page.Category
                , navLink page Route.Area <| Page.toTitle Page.Area
                , navLink page Route.Reference <| Page.toTitle Page.Reference
                ]
            , B.navbarEnd []
                [ B.navbarItem True
                    []
                    [ div [ class "buttons" ]
                        [ a
                            [ class "button is-info"
                            , href "https://github.com/HirotoShioi/butterfly-elm"
                            ]
                            [ text "Github" ]
                        ]
                    ]
                ]
            ]
        ]


navLink : Page -> Route -> String -> Html msg
navLink page route linkName =
    B.navbarItemLink (isActive page route) [ Route.href route ] [ text linkName ]


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

        ( Page.Dictionary, Route.Dictionary _ ) ->
            True

        _ ->
            False


myNavbarBurger : Bool -> Html Msg
myNavbarBurger isMenuOpen =
    let
        activeClass =
            if isMenuOpen then
                "is-active"

            else
                ""
    in
    div
        [ class "navbar-burger"
        , onClick ToggleMenu
        , class activeClass
        ]
        [ span [] []
        , span [] []
        , span [] []
        ]
