module Page.Dictionary exposing (Model, Msg(..), getKey, getSession, init, update, updateSession, view)

import Browser.Navigation as Nav
import Bulma.Components as Components
import Butterfly.Query as Query exposing (Query, filterButterflies)
import Butterfly.Type exposing (Butterfly, Region(..), fromRegion, regionList, toRegion)
import Html exposing (Html, a, div, p, text)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Page.Dictionary.View as View
import Session exposing (Session)
import Set exposing (Set)
import Util exposing (updateWith)


type Msg
    = ToggleRegionMenu
    | RegionClicked String
    | ToggleCategoryMenu
    | CategoryClicked String
    | ToggleColorMenu
    | ColorClicked String
    | ResetColor
    | ResetRegion
    | ResetCategory
    | GotSessionMsg Session.Msg


type alias Model =
    { session : Session
    , isRegionMenuOpen : Bool
    , isCategoryMenuOpen : Bool
    , isColorMenuOpen : Bool
    }


getKey : Model -> Nav.Key
getKey model =
    model.session.key


getSession : Model -> Session
getSession model =
    model.session


disableMenus : Model -> Model
disableMenus model =
    Model model.session False False False


init : Session -> ( Model, Cmd Msg )
init session =
    initResult session


initResult : Session -> ( Model, Cmd Msg )
initResult session =
    ( Model session False False False, Cmd.none )


updateSession : Model -> Session -> Model
updateSession model session =
    { model | session = session }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CategoryClicked category ->
            let
                updatedModel =
                    disableMenus model
            in
            Session.update (Session.FromDictionary <| Query.UpdateCategory category) model.session
                |> updateWith (updateSession updatedModel) GotSessionMsg

        RegionClicked regionStr ->
            case toRegion regionStr of
                Err _ ->
                    ( model, Cmd.none )

                Ok region ->
                    let
                        updatedModel =
                            disableMenus model
                    in
                    Session.update (Session.FromDictionary <| Query.UpdateRegion region) model.session
                        |> updateWith (updateSession updatedModel) GotSessionMsg

        ColorClicked hexString ->
            let
                updatedModel =
                    disableMenus model
            in
            Session.update (Session.FromDictionary <| Query.UpdateColor hexString) model.session
                |> updateWith (updateSession updatedModel) GotSessionMsg

        ToggleRegionMenu ->
            let
                updatedModel =
                    { model
                        | isRegionMenuOpen = not model.isRegionMenuOpen
                        , isCategoryMenuOpen = False
                        , isColorMenuOpen = False
                    }
            in
            ( updatedModel, Cmd.none )

        ToggleCategoryMenu ->
            let
                updatedModel =
                    { model
                        | isCategoryMenuOpen = not model.isCategoryMenuOpen
                        , isRegionMenuOpen = False
                        , isColorMenuOpen = False
                    }
            in
            ( updatedModel, Cmd.none )

        ToggleColorMenu ->
            let
                updatedModel =
                    { model
                        | isColorMenuOpen = not model.isColorMenuOpen
                        , isRegionMenuOpen = False
                        , isCategoryMenuOpen = False
                    }
            in
            ( updatedModel, Cmd.none )

        ResetColor ->
            Session.update (Session.FromDictionary Query.ResetColor) model.session
                |> updateWith (updateSession model) GotSessionMsg

        ResetRegion ->
            Session.update (Session.FromDictionary Query.ResetRegion) model.session
                |> updateWith (updateSession model) GotSessionMsg

        ResetCategory ->
            Session.update (Session.FromDictionary Query.ResetCategory) model.session
                |> updateWith (updateSession model) GotSessionMsg

        GotSessionMsg sessionMsg ->
            Session.update sessionMsg model.session
                |> updateWith (updateSession model) GotSessionMsg



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    case model.session.butterflies of
        Ok butterflies ->
            let
                filteredButterflies =
                    filterButterflies butterflies model.session.query

                categoryDropdown =
                    mkCategoryDropdown butterflies model

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
                                ( butterfly.jpName, View.showButterflies butterfly )
                            )
                            filteredButterflies
                ]

        Err _ ->
            View.errorView


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


mkRegionDropdown : Model -> Html Msg
mkRegionDropdown model =
    let
        regionNavList =
            List.map fromRegion regionList
    in
    View.searchDropdown "生息地" regionNavList ToggleRegionMenu model.isRegionMenuOpen False RegionClicked


mkCategoryDropdown : List Butterfly -> Model -> Html Msg
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


mkColorDropdown : Model -> msg -> (String -> msg) -> Html msg
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
