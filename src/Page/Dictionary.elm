module Page.Dictionary exposing (Model, Msg(..), getKey, getSession, init, title, update, updateSession, view)

import Browser.Navigation as Nav
import Bulma.Components as Components
import Butterfly.Api as Api exposing (Msg(..))
import Butterfly.Type exposing (Butterfly, Query, Region(..), filterButterflies, fromRegion, toRegion)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Page
import Page.Dictionary.View as View
import Route
import Session exposing (Session)
import Set exposing (Set)


type Msg
    = GotButterflyResponse Api.Msg
    | ButterflyClicked Butterfly
    | ToggleRegionMenu
    | RegionClicked String
    | ToggleCategoryMenu
    | CategoryClicked String
    | ToggleColorMenu
    | ColorClicked String
    | ResetColor
    | ResetRegion
    | ResetCategory


type Model
    = Loading Session
    | Result ResultModel


type alias ResultModel =
    { session : Session
    , butterflies : List Butterfly
    , isRegionMenuOpen : Bool
    , isCategoryMenuOpen : Bool
    , isColorMenuOpen : Bool
    }


title : String
title =
    Page.dictionaryTitle


disableMenus : ResultModel -> ResultModel
disableMenus resultModel =
    ResultModel resultModel.session resultModel.butterflies False False False


init : Session -> ( Model, Cmd Msg )
init session =
    ( Loading session, Cmd.map GotButterflyResponse Api.getButterflies )


initResult : Session -> List Butterfly -> ( Model, Cmd Msg )
initResult session butterflies =
    ( Result <| ResultModel session butterflies False False False, Cmd.none )


getKey : Model -> Nav.Key
getKey model =
    case model of
        Loading s ->
            s.key

        Result m ->
            m.session.key


getSession : Model -> Session
getSession model =
    case model of
        Loading s ->
            s

        Result m ->
            m.session


updateSession : Model -> Session -> Model
updateSession model session =
    case model of
        Loading _ ->
            Loading session

        Result m ->
            Result { m | session = session }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotButterflyResponse apiMsg, _ ) ->
            case apiMsg of
                Api.GotButterflies (Ok butterflies) ->
                    let
                        session =
                            getSession model

                        bs =
                            butterflies
                    in
                    initResult session bs

                Api.GotButterflies (Err _) ->
                    let
                        key =
                            getKey model

                        route =
                            Route.routeToString Route.Error
                    in
                    ( model, Nav.pushUrl key route )

        ( ButterflyClicked butterfly, Result m ) ->
            let
                newSession =
                    Session.update (Session.EnableModal butterfly) m.session
            in
            ( updateSession model newSession, Cmd.none )

        ( CategoryClicked category, Result m ) ->
            let
                newSession =
                    Session.update (Session.AddCategory category) m.session

                updatedModel =
                    disableMenus m
            in
            ( updateSession (Result updatedModel) newSession, Cmd.none )

        ( RegionClicked regionStr, Result m ) ->
            case toRegion regionStr of
                Err _ ->
                    ( model, Cmd.none )

                Ok region ->
                    let
                        newSession =
                            Session.update (Session.UpdateRegion region) m.session

                        updatedModel =
                            disableMenus m
                    in
                    ( updateSession (Result updatedModel) newSession, Cmd.none )

        ( ColorClicked hexString, Result m ) ->
            let
                newSession =
                    Session.update (Session.UpdateColor hexString) m.session

                updatedModel =
                    disableMenus m
            in
            ( updateSession (Result updatedModel) newSession, Cmd.none )

        ( ToggleRegionMenu, Result m ) ->
            let
                updatedModel =
                    { m | isRegionMenuOpen = not m.isRegionMenuOpen, isCategoryMenuOpen = False, isColorMenuOpen = False }
            in
            ( Result updatedModel, Cmd.none )

        ( ToggleCategoryMenu, Result m ) ->
            let
                updatedModel =
                    { m | isCategoryMenuOpen = not m.isCategoryMenuOpen, isRegionMenuOpen = False, isColorMenuOpen = False }
            in
            ( Result updatedModel, Cmd.none )

        ( ToggleColorMenu, Result m ) ->
            let
                updatedModel =
                    { m | isColorMenuOpen = not m.isColorMenuOpen, isRegionMenuOpen = False, isCategoryMenuOpen = False }
            in
            ( Result updatedModel, Cmd.none )

        ( ResetColor, Result m ) ->
            let
                newSession =
                    Session.update Session.ResetColor m.session
            in
            ( updateSession (Result m) newSession, Cmd.none )

        ( ResetRegion, Result m ) ->
            let
                newSession =
                    Session.update Session.ResetRegion m.session
            in
            ( updateSession (Result m) newSession, Cmd.none )

        ( ResetCategory, Result m ) ->
            let
                newSession =
                    Session.update Session.ResetCategory m.session
            in
            ( updateSession (Result m) newSession, Cmd.none )

        _ ->
            ( model, Cmd.none )



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            View.loadingView

        Result m ->
            resultView m


resultView : ResultModel -> Html Msg
resultView model =
    let
        filteredButterflies =
            filterButterflies model.butterflies model.session.query

        categoryDropdown =
            mkCategoryDropdown model.butterflies model

        regionDropdown =
            mkRegionDropdown model

        colorDropdown =
            mkColorDropdown model
    in
    div [ class "dictionary-view" ]
        [ div [ class "dictionary-search-content" ]
            [ div [ class "field is-grouped is-grouped-multiline" ]
                [ div [ class "control" ] [ regionDropdown ]
                , div [ class "control" ] [ categoryDropdown ]
                , div [ class "control" ] [ colorDropdown ToggleColorMenu ColorClicked ]
                ]
            , tagList model.session.query
            ]
        , if List.isEmpty filteredButterflies then
            View.emptyView

          else
            Keyed.node "div" [ class "columns  is-multiline" ] <|
                List.map
                    (\butterfly ->
                        ( butterfly.jpName, View.showButterflies butterfly ButterflyClicked )
                    )
                    filteredButterflies
        ]


tagList : Query -> Html Msg
tagList query =
    let
        mkTag mValue func =
            Maybe.withDefault [] <| Maybe.map (\str -> List.singleton <| func str) mValue

        cTag =
            mkTag query.hexColor (View.colorTag ResetColor)

        categoryTag =
            mkTag query.category (View.searchTag ResetCategory)

        regionTag =
            mkTag (Maybe.map fromRegion query.region) (View.searchTag ResetRegion)

        list =
            List.concat [ regionTag, categoryTag, cTag ]
    in
    if List.isEmpty <| list then
        div [] []

    else
        div [ class "field is-grouped is-grouped-multiline" ] <|
            List.map
                (\ele -> div [ class "control" ] [ ele ])
                list


mkRegionDropdown : ResultModel -> Html Msg
mkRegionDropdown model =
    let
        regionList =
            List.map fromRegion [ OldNorth, NewNorth, NewTropical, TropicalAfrica, IndiaAustralia ]
    in
    View.searchDropdown "生息地" regionList ToggleRegionMenu model.isRegionMenuOpen False RegionClicked


mkCategoryDropdown : List Butterfly -> ResultModel -> Html Msg
mkCategoryDropdown butterflies model =
    let
        categoryList =
            Set.toList <| mkCategorySet model.session.query butterflies
    in
    View.searchDropdown
        "分類"
        categoryList
        ToggleCategoryMenu
        model.isCategoryMenuOpen
        (List.isEmpty categoryList)
        CategoryClicked


mkCategorySet : Query -> List Butterfly -> Set String
mkCategorySet query butterflies =
    let
        cQuery =
            { query | category = Nothing }

        filteredButterflies =
            filterButterflies butterflies cQuery
    in
    List.foldr (\butterfly set -> Set.insert butterfly.category set) Set.empty filteredButterflies


mkColorDropdown : ResultModel -> msg -> (String -> msg) -> Html msg
mkColorDropdown resultModel toggleMsg clickedMsg =
    let
        colorList =
            [ "#e60012"
            , "#f39800"
            , "#fff100"
            , "#8fc31f"
            , "#009944"
            , "#009e96"
            , "#00a0e9"
            , "#0068b7"
            , "#1d2088"
            , "#920783"
            , "#e4007f"
            , "#e5004f"
            ]
    in
    Components.dropdown resultModel.isColorMenuOpen
        Components.dropdownModifiers
        []
        [ View.searchDropdownTrigger toggleMsg "色" False
        , Components.dropdownMenu []
            [ class "color-palette-wrapper" ]
            [ Components.dropdownItem False
                []
                [ div [ class "color-palette field is-grouped is-grouped-multiline" ] <|
                    List.map
                        (\hexColor ->
                            div
                                [ class "control color-palette-item"
                                , style "background-color" hexColor
                                , onClick (clickedMsg hexColor)
                                ]
                                []
                        )
                        colorList
                ]
            ]
        ]
